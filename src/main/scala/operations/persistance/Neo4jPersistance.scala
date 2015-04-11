package operations.persistance

import models._
import org.neo4j.graphdb.Node
import org.neo4j.graphdb._
import org.neo4j.graphdb.factory.GraphDatabaseFactory

//embedded neo4j database can be open one at the time, so the synhronization hasn't be done
object Neo4j {
  private var graph: GraphDatabaseService = null

  def openConnection(): Unit = {
    graph = new GraphDatabaseFactory()
      .newEmbeddedDatabase("neo4j-community-2.2.0/data/graph.db")
  }

  def closeConnection(): Unit = {
    graph.shutdown()
    graph = null
  }

  def persistTag(tag: Tag): Unit = {
    val transaction: Transaction = graph.beginTx()
    try {
      val node: Node = graph.createNode()
      node.addLabel(DynamicLabel.label("Tag"))
      node.setProperty("name", tag.name)
      transaction.success()
    } catch  {
      case e: ConstraintViolationException => transaction.failure()
    } finally {
      transaction.close()
    }
  }

  def persistTags(tags: List[Tag]): Unit = {
    for (tag <- tags) {
      persistTag(tag)
    }
  }

  private def findNodes(nodeName: String): ResourceIterator[Node] = {
    val transaction: Transaction = graph.beginTx()
    try {
      val nodes = graph.findNodes(DynamicLabel.label(nodeName))
      transaction.success()
      nodes
    } finally {
      transaction.close()
    }
  }

  private def findNode(nodeName: String, property: String, key: String): Node = {
    val transaction: Transaction = graph.beginTx()
    try {
      val node = graph.findNode(DynamicLabel.label(nodeName), property, key)
      transaction.success()
      node
    } finally {
      transaction.close()
    }
  }

  def persistRelatedTags(tagName: String, relatedTags: List[Tag]): Unit = {
    val mainNode: Node = findNode("Tag", "name", tagName)
    val rel = new RelatedTagsEdge
    for (tag <- relatedTags) {
      persistTag(tag)
      val transaction: Transaction = graph.beginTx()
      try {
        val relatedNode: Node = graph.findNode(DynamicLabel.label("Tag"), "name", tag.name)
        mainNode.createRelationshipTo(relatedNode, rel)
        transaction.success()
      } finally {
        transaction.close()
      } 
    }
  }

  def persistSynonymTags(synonymsTags: List[SynonymTagsEdge]): Unit = {
    if (synonymsTags.isEmpty) {
      return
    }
    val mainNode: Node = findNode("Tag", "name", synonymsTags(0).to_tag)
    for (tag <- synonymsTags) {
      //persist node to which relationship should connect
      persistTag(new Tag(tag.from_tag))
      //persist relationship
      val transaction: Transaction = graph.beginTx()
      try {
        val synonymNode: Node = graph.findNode(DynamicLabel.label("Tag"), "name", tag.from_tag)
        val relationship: Relationship = mainNode.createRelationshipTo(synonymNode, tag)
        relationship.setProperty("count", tag.applied_count)
        transaction.success()
      } finally {
        transaction.close()
      }
    }
  }

  def persistQuestion(question: Question): Unit = {
    val transaction: Transaction = graph.beginTx()
    try {
      val node: Node = graph.createNode()
      node.addLabel(DynamicLabel.label("Question"))
      node.setProperty("link", question.link)
      node.setProperty("score", question.score)
      node.setProperty("answer_count", question.answer_count)
      node.setProperty("is_answered", question.is_answered)
      node.setProperty("view_count", question.view_count)
      node.setProperty("question_id", question.question_id.toString)
      transaction.success()
    } catch  {
      case e: ConstraintViolationException => transaction.failure()
    } finally {
      transaction.close()
    }
  }

  def persistQuestions(questions: List[Question]): Unit = {
    for (question <- questions) persistQuestion(question)
  }

  def persistFAQ(tagName:String, questions: List[Question]): Unit = {
    val mainNode: Node = findNode("Tag", "name", tagName)
    val rel = new FAQEdge
    for (question <- questions) {
      persistQuestion(question)
      val transaction: Transaction = graph.beginTx()
      try {
        val relatedNode: Node =
          graph.findNode(DynamicLabel.label("Question"), "question_id", question.question_id)
        mainNode.createRelationshipTo(relatedNode, rel)
        transaction.success()
      } finally {
        transaction.close()
      }
    }
  }

  def persistRelatedQuestions(question:Question, questions: List[Question]): Unit = {
    val mainNode: Node = findNode("Question", "question_id", question.question_id.toString)
    val rel = new RelatedQuestionsEdge
    if (questions != null) {
      println(questions)
      for (q <- questions) {
        persistQuestion(q)
        val transaction: Transaction = graph.beginTx()
        try {
          val relatedNode: Node =
            graph.findNode(DynamicLabel.label("Question"), "question_id", q.question_id.toString)
          mainNode.createRelationshipTo(relatedNode, rel)
          transaction.success()
        } finally {
          transaction.close()
        }
      }
    }
  }

  @Deprecated
  def extractTags: List[Tag] = {
    val nodes = findNodes("Tag")
    var tags: List[Tag] = List()
    while (nodes.hasNext) {
      val node = nodes.next()
      val transaction: Transaction = graph.beginTx()
      try {
        tags = new Tag(node.getProperty("name").toString) :: tags
        transaction.success()
      } finally {
        transaction.close()
      }
    }
    nodes.close()
    tags
  }

  def extractSynonymTagsIncludingMe(tagName: String): List[Tag] = {
    val transaction: Transaction = graph.beginTx()
    try {
      val query =
        """match (n:Tag)-[:`models.SynonymTagsEdge`]-(t:Tag)
          |where n.name="""".stripMargin + tagName + """"
          |return t""".stripMargin
      val result: Result = graph.execute(query)
      val iterator: ResourceIterator[Node] = result.columnAs("t")
      var synonyms: List[Tag] = List(new Tag(tagName))
      while (iterator.hasNext) {
        val node: Node = iterator.next()
        val tag: Tag = new Tag(node.getProperty("name").toString)
        synonyms = tag :: synonyms
      }
      transaction.success()
      synonyms
    } finally {
      transaction.close()
    }
  }

  def extractFAQOfTag(tag: String): List[Question] = {
    val transaction: Transaction = graph.beginTx()
    try {
      val query =
        """match (t:Tag)-[:`models.FAQEdge`]-(q:Question)
          |where t.name="""".stripMargin + tag + """"
          |return distinct q""".stripMargin
      val result: Result = graph.execute(query)
      val iterator: ResourceIterator[Node] = result.columnAs("q")
      var faq: List[Question] = List()
      while (iterator.hasNext) {
        val node: Node = iterator.next()
        val question_id: Int = node.getProperty("question_id").toString.toInt
        val link: String = node.getProperty("link").toString
        val score: Int = node.getProperty("score").toString.toInt
        val answer_count: Int = node.getProperty("answer_count").toString.toInt
        val is_answered: Boolean = node.getProperty("is_answered").toString.toBoolean
        val view_count: Int = node.getProperty("view_count").toString.toInt

        val question = new Question(question_id = question_id, link = link, score = score,
          answer_count = answer_count, is_answered = is_answered, view_count = view_count)
        faq = question :: faq
      }
      transaction.success()
      faq
    } finally {
      transaction.close()
    }
  }

  def extractRelatedQuestions(tag: String, depth: Int): List[Question] = {
    if (depth <= 0) throw new IllegalArgumentException("Depth must be greater than 0")
    val transaction: Transaction = graph.beginTx()
    try {
      //making query
      var query = "match (t:Tag)-[:`models.FAQEdge`]-"
      for (i <- 1 to depth) {
        query += "()-[:`models.RelatedQuestionsEdge`]-"
      }
      query +=
        """(q:Question)
          |where t.name="""".stripMargin + tag + """"
          |return distinct q""".stripMargin
      val result: Result = graph.execute(query)
      val iterator: ResourceIterator[Node] = result.columnAs("q")
      var questions: List[Question] = List()
      while (iterator.hasNext) {
        val node: Node = iterator.next()
        val question_id: Int = node.getProperty("question_id").toString.toInt
        val link: String = node.getProperty("link").toString
        val score: Int = node.getProperty("score").toString.toInt
        val answer_count: Int = node.getProperty("answer_count").toString.toInt
        val is_answered: Boolean = node.getProperty("is_answered").toString.toBoolean
        val view_count: Int = node.getProperty("view_count").toString.toInt

        val question = new Question(question_id = question_id, link = link, score = score,
          answer_count = answer_count, is_answered = is_answered, view_count = view_count)
        questions = question :: questions
      }
      transaction.success()
      questions
    } finally {
      transaction.close()
    }
  }

  def executeNetworkClosureQuery(query:String): Long = {
    val transaction: Transaction = graph.beginTx()
    try {
      val result: Result = graph.execute(query)
      val count: Long = result.columnAs("count(distinct n)").next()
      transaction.success()
      count
    } finally {
      transaction.close()
    }
  }

  def executeCountOfUnion(query:String): Long = {
    val transaction: Transaction = graph.beginTx()
    try {
      val result: Result = graph.execute(query)
      var count: Long = 0
      while (result.hasNext) {
        count += 1
        result.next()
      }
      transaction.success()
      count
    } finally {
      transaction.close()
    }
  }
}
