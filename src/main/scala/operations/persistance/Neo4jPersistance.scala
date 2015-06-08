package operations.persistance

import models._
import org.neo4j.graphdb.Node
import org.neo4j.graphdb._
import org.neo4j.graphdb.factory.GraphDatabaseFactory

import scala.collection.mutable.ListBuffer

/**
 * Class for embedded neo4j database can be open one at the time, so the additional synchronization options haven't be done,
 * as they are not needed.
 */
object Neo4j {
  /**
   * Graph used for persisting objects and analysis of social network.
   */
  private var graph: GraphDatabaseService = null

  /**
   * Method for opening connection to the Neo4j database.
   * <p>Note:</p>
   * Only one connection can be opened at time.
   */
  def openConnection(): Unit = {
    graph = new GraphDatabaseFactory()
      .newEmbeddedDatabase("neo4j-community-2.2.0/data/graph.db")
  }

  /**
   * Method for closing connection to the Neo4j database.
   */
  def closeConnection(): Unit = {
    graph.shutdown()
    graph = null
  }

  /**
   * Method for persisting tag to the Neo4j.
   * @param tag
   */
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

  /**
   * Method for persisting set of tags to the Neo4j. It persists each in separate transaction.
   * @param tags
   */
  def persistTags(tags: List[Tag]): Unit = {
    for (tag <- tags) {
      persistTag(tag)
    }
  }

  /**
   * Method used internally for finding node with specific attributes. Not safe to use inside other transaction.
   * @param labelName name of node label
   * @param key key by which nodes are sorted
   * @param value value of noted key
   * @return specified node
   */
  private def findNode(labelName: String, key: String, value: String): Node = {
    val transaction: Transaction = graph.beginTx()
    try {
      val node = graph.findNode(DynamicLabel.label(labelName), key, value)
      transaction.success()
      node
    } finally {
      transaction.close()
    }
  }

  /**
   * Method for persisting related tags. It persist related tags and create edge between tag with tagName value for
   * key name, and all related tags. Tag with tagName should be previously persisted.
   * @param tagName
   * @param relatedTags
   */
  def persistRelatedTags(tagName: String, relatedTags: List[Tag]): Unit = {
    val mainNode: Node = findNode("Tag", "name", tagName)
    if (mainNode == null) throw new IllegalArgumentException(tagName + " has not be persisted yet.")
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

  /**
   * Method for persisting synonym edges.
   * @param synonymsTags
   */
  def persistSynonymTags(synonymsTags: List[SynonymTagsEdge]): Unit = {
    if (synonymsTags.isEmpty) return
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

  /**
   * Method for persisting question. The methods also persist belongs methods between given question and tags inside it.
   * @param question question that needs persisting
   */
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
      node.setProperty("last_activity_date", question.last_activity_date)
      node.setProperty("creation_date", question.creation_date)
      transaction.success()
    } catch  {
      case e: ConstraintViolationException => transaction.failure()
    } finally {
      transaction.close()
    }
    //implement belongs questions
    if (question.tags != null) {
      val relationshipType = new BelongsEdge
      for (tagName <- question.tags) {
        persistTag(new Tag(tagName))
        val questionNode: Node = findNode("Question", "question_id", question.question_id.toString)
        val transaction: Transaction = graph.beginTx()
        try {
          val tagNode: Node = graph.findNode(DynamicLabel.label("Tag"), "name", tagName)
          tagNode.createRelationshipTo(questionNode, relationshipType)
          transaction.success()
        } finally {
          transaction.close()
        }
      }
    }
  }

  /**
   * Method for persisting set of questions to the Neo4j. It persists each in separate transaction.
   * @param questions
   */
  def persistQuestions(questions: List[Question]): Unit = {
    for (question <- questions) persistQuestion(question)
  }

  def persistUser(user: User): Unit = {
    val transaction: Transaction = graph.beginTx()
    try {
      val node: Node = graph.createNode()
      node.addLabel(DynamicLabel.label("User"))
      node.setProperty("user_id", user.user_id.toString)
      transaction.success()
    } catch  {
      case e: ConstraintViolationException => transaction.failure()
    } finally {
      transaction.close()
    }
  }

  def persistUsers(users: List[User]): Unit = {
    for (user <- users) persistUser(user)
  }

  /**
   * Method for persisting Frequently Asked Questions and appropriate edges. Note: tag must be previously persisted.
   * @param tagName
   * @param questions frequently asked questions
   */
  def persistFAQ(tagName:String, questions: List[Question]): Unit = {
    if (questions == null) return
    val mainNode: Node = findNode("Tag", "name", tagName)
    if (mainNode == null) throw new IllegalArgumentException(tagName + " has not be persisted.")
    val rel = new FAQEdge
    for (question <- questions) {
      persistQuestion(question)
      val transaction: Transaction = graph.beginTx()
      try {
        val relatedNode: Node =
          graph.findNode(DynamicLabel.label("Question"), "question_id", question.question_id.toString)
        mainNode.createRelationshipTo(relatedNode, rel)
        transaction.success()
      } finally {
        transaction.close()
      }
    }
  }

  /**
   * Method for persisting similar tags edge. Note: both tags should be previously persisted.
   * @param tag1
   * @param tag2
   */
  def persistSimilarTagsEdge(tag1: String, tag2: String): Unit = {
    val node1: Node = findNode("Tag", "name", tag1)
    val node2: Node = findNode("Tag", "name", tag2)
    val rel = new SimilarTagsEdge
    val transaction: Transaction = graph.beginTx()
    try {
      node1.createRelationshipTo(node2, rel)
      transaction.success()
    } finally {
      transaction.close()
    }
  }

  /**
   * Method for persisting top askers for given tag.
   * @param users
   * @param tag
   */
  def persistTopAskers(users: List[User], tag: String): Unit = {
    persistUsers(users)
    val node1: Node = findNode("Tag", "name", tag)
    val rel = new TopAskersEdge
    for (user <- users) {
      val node2: Node = findNode("User", "user_id", user.user_id.toString)
      val transaction: Transaction = graph.beginTx()
      try {
        node1.createRelationshipTo(node2, rel)
        transaction.success()
      } finally {
        transaction.close()
      }
    }
  }

  /**
   * Method for persisting top tags for given user.
   * @param tags
   * @param user
   */
  def persistTopTags(tags:List[Tag], user: User): Unit = {
    persistTags(tags)
    val node1: Node = findNode("User", "user_id", user.user_id.toString)
    val rel = new TopTagsEdge
    for (tag <- tags) {
      val node2: Node = findNode("Tag", "name", tag.name)
      val transaction: Transaction = graph.beginTx()
      try {
        node1.createRelationshipTo(node2, rel)
        transaction.success()
      } finally {
        transaction.close()
      }
    }
  }

  /**
   * Method for creating pmi connections. Note: both tags should be previously persisted.
   * @param tag1
   * @param tag2
   * @param pmi
   */
  def persistPMI(tag1: String, tag2: String, pmi: Double): Unit = {
    val node1: Node = findNode("Tag", "name", tag1)
    val node2: Node = findNode("Tag", "name", tag2)
    val rel = new PMITagsEdge(pmi)
    val transaction: Transaction = graph.beginTx()
    try {
      val relationship = node1.createRelationshipTo(node2, rel)
      relationship.setProperty("weight", pmi)
      transaction.success()
    } finally {
      transaction.close()
    }
  }

  /**
   * Method for persisting related questions.
   * @param question question to which all other questions are related
   * @param questions other questions
   */
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

  /**
   * Method for extracting from database Synonym tags including the given tagName
   * @param tagName tag for which synonym tags should be extracted.
   * @return synonym tags and tag with tagName
   */
  def extractSynonymTagsIncludingMe(tagName: String): List[Tag] = {
    val transaction: Transaction = graph.beginTx()
    try {
      val query =
        """match (n:Tag)-[:`models.SynonymTagsEdge`]-(t:Tag)
          |where n.name="""".stripMargin + tagName + """"
          |return distinct t""".stripMargin
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

  /**
   * Method for extracting all tags that belongs to specific question.
   * @param question_id
   * @return
   */
  def extractBelongsTags(question_id: String): List[Tag] = {
    val transaction: Transaction = graph.beginTx()
    try {
      val query =
        """match (t:Tag)-[:`models.BelongsEdge`]-(q:Question)
          |where q.question_id="""".stripMargin + question_id + """"
          |return distinct t""".stripMargin
      val result: Result = graph.execute(query)
      val iterator: ResourceIterator[Node] = result.columnAs("t")
      var belongs: List[Tag] = List()
      while (iterator.hasNext) {
        val node: Node = iterator.next()
        val tag: Tag = new Tag(node.getProperty("name").toString)
        belongs = tag :: belongs
      }
      transaction.success()
      belongs
    } finally {
      transaction.close()
    }
  }

  def extractTopAskers(tagName: String): List[User] = {
    val transaction: Transaction = graph.beginTx()
    try {
      val query =
        """match (t:Tag)-[:`models.TopAskersEdge`]-(u:User)
          |where t.name="""".stripMargin + tagName + """"
          |return distinct u""".stripMargin
      val result: Result = graph.execute(query)
      val iterator: ResourceIterator[Node] = result.columnAs("u")
      val topAskers: ListBuffer[User] = ListBuffer.empty
      while (iterator.hasNext) {
        val node: Node = iterator.next()
        topAskers += new User(node.getProperty("user_id").toString.toLong)
      }
      transaction.success()
      topAskers.result()
    } finally {
      transaction.close()
    }
  }

  def extractPMI(tagName1: String, tagName2: String): PMITagsEdge = {
    val transaction: Transaction = graph.beginTx()
    try {
      val query =
        """match (t:Tag)-[r:`models.PMITagsEdge`]-(t2:Tag)
          |where t.name="""".stripMargin + tagName1 + """" and t2.name="""" + tagName2 + """"
          |return distinct r""".stripMargin
      val result: Result = graph.execute(query)
      val iterator: ResourceIterator[Relationship] = result.columnAs("r")
      var ret: PMITagsEdge = null
      if (iterator.hasNext) {
        val relationship: Relationship = iterator.next()
        ret = new PMITagsEdge(relationship.getProperty("weight").toString.toDouble)
      }
      transaction.success()
      ret
    } finally {
      transaction.close()
    }
  }

  def extractTopTags(user_id: String): List[Tag] = {
    val transaction: Transaction = graph.beginTx()
    try {
      val query =
        """match (t:Tag)-[:`models.TopTagsEdge`]-(u:User)
          |where u.user_id="""".stripMargin + user_id + """"
          |return distinct t""".stripMargin
      val result: Result = graph.execute(query)
      val iterator: ResourceIterator[Node] = result.columnAs("t")
      var topTags: List[Tag] = List()
      while (iterator.hasNext) {
        val node: Node = iterator.next()
        val tag: Tag = new Tag(node.getProperty("name").toString)
        topTags = tag :: topTags
      }
      transaction.success()
      topTags
    } finally {
      transaction.close()
    }
  }

  /**
   * Method for internally converting node data to question.
   * @param node
   * @return
   */
  private def deserialize(node: Node): Question = {
    val question_id: Int = node.getProperty("question_id").toString.toInt
    val link: String = node.getProperty("link").toString
    val score: Double = node.getProperty("score").toString.toDouble
    val answer_count: Double = node.getProperty("answer_count").toString.toDouble
    val is_answered: Boolean = node.getProperty("is_answered").toString.toBoolean
    val view_count: Double = node.getProperty("view_count").toString.toDouble
    val last_activity_date: Double = node.getProperty("last_activity_date").toString.toDouble
    val creation_date: Double = node.getProperty("creation_date").toString.toDouble
    new Question(
      question_id = question_id,
      link = link,
      score = score,
      answer_count = answer_count,
      is_answered = is_answered,
      view_count = view_count,
      last_activity_date = last_activity_date,
      creation_date = creation_date
    )
  }

  /**
   * Method for extracting all questions from database.
   * @return
   */
  def extractQuestions(): List[Question] = {
    val transaction: Transaction = graph.beginTx()
    try {
      val iterator: ResourceIterator[Node] = graph.findNodes(DynamicLabel.label("Question"))
      var questions: List[Question] = List()
      while (iterator.hasNext) {
        val node: Node = iterator.next()
        val question = deserialize(node)
        questions = question :: questions
      }
      transaction.success()
      questions
    } finally {
      transaction.close()
    }
  }

  /**
   * Method for extracting all tags from database.
   * @return
   */
  def extractTags(): List[Tag] = {
    val transaction: Transaction = graph.beginTx()
    try {
      val iterator: ResourceIterator[Node] = graph.findNodes(DynamicLabel.label("Tag"))
      var tags: List[Tag] = List()
      while (iterator.hasNext) {
        val node: Node = iterator.next()
        val tag = new Tag(node.getProperty("name").toString)
        tags = tag :: tags
      }
      transaction.success()
      tags
    } finally {
      transaction.close()
    }
  }

  def extractClusterForTag(tagName: String): List[Tag] = {
    val query =
      """match (t:Tag)-[]-(n:Tag)
        |where t.name="""".stripMargin + tagName + """"
        |return n""".stripMargin
    println(query)
    val transaction: Transaction = graph.beginTx()
    try {
      val result: Result = graph.execute(query)
//      println(result.resultAsString())
      val iterator: ResourceIterator[Node] = result.columnAs("n")
      val cluster: ListBuffer[Tag] = ListBuffer.empty
      cluster += new Tag(tagName)
      while (iterator.hasNext) {
        val node: Node = iterator.next()
        val tag = new Tag(node.getProperty("name").toString)
        cluster += tag
      }
      transaction.success()
      cluster.result()
    } finally {
      transaction.close()
    }
  }

  /**
   * Method for extracting questions for a tag at certain depth.
   * @param tag
   * @param depth
   * @return
   */
  def extractQuestionsAtDepth(tag: String, depth: Int): List[Question] = {
    if (depth < 0) throw new IllegalArgumentException("Depth must be greater or equal than 0")
    val transaction: Transaction = graph.beginTx()
    try {
      //making query
      var query = "match (t:Tag)-[]-"
      for (i <- 1 to depth) {
        query += "()-[]-"
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
        val question = deserialize(node)
        questions = question :: questions
      }
      transaction.success()
      questions
    } finally {
      transaction.close()
    }
  }

  /**
   * Method for executing query that return number.
   * @param query
   * @return count(distinct n)
   */
  def executeCountQuery(query:String): Long = {
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

  /**
   * Method for counting union.
   * @param query
   * @return
   */
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

  def averagePMIValue: Double = {
    val transaction: Transaction = graph.beginTx()
    try {
      val query =
        """match (:Tag)-[r:`models.PMITagsEdge`]-(:Tag)
          |return distinct r""".stripMargin
      val result: Result = graph.execute(query)
      val iterator: ResourceIterator[Relationship] = result.columnAs("r")
      var total: Double = 0
      var count: Long = 0
      while (iterator.hasNext) {
        val relationship: Relationship = iterator.next()
        total += relationship.getProperty("weight").toString.toDouble
        count += 1
      }
      transaction.success()
      total / count
    } finally {
      transaction.close()
    }
  }

  /**
   * Check if question belong to certain tag.
   * @param question_id unique id of question
   * @param tagName unique name of tag
   * @return
   */
  def ifQuestionBelongToTag(question_id: String, tagName: String): Boolean = {
    val query =
      """match (t:Tag)-[r:`models.BelongsEdge`]-(q:Question)
        |where t.name="""".stripMargin + tagName + """" and q.question_id = """" + question_id + """"
        |return count(distinct r)""".stripMargin
    val transaction: Transaction = graph.beginTx()
    try {
      val result: Result = graph.execute(query)
      val value = result.columnAs[Long]("count(distinct r)").next()
      transaction.success()
      value > 0
    } finally {
      transaction.close()
    }
  }

  def ifQuestionIsInRelationshipWithTag(question_id: String, tagName: String): Boolean = {
    val query =
      """match (t:Tag)-[r]-(q:Question)
        |where t.name="""".stripMargin + tagName + """" and q.question_id = """" + question_id + """"
        |return count(distinct r)""".stripMargin
    val transaction: Transaction = graph.beginTx()
    try {
      val result: Result = graph.execute(query)
      val value = result.columnAs[Long]("count(distinct r)").next()
      transaction.success()
      value > 0
    } finally {
      transaction.close()
    }
  }

  private def doesNodeHaveRelationship(nodeType: String, nodeIdName: String, nodeId: String, relationshipName: String): Boolean = {
    val query =
      """match (node:""" + nodeType + """)-[r:`""" + relationshipName +"""`]-()
        |where node.""".stripMargin + nodeIdName + """="""" + nodeId + """"
        |return count(distinct r)""".stripMargin
    val transaction: Transaction = graph.beginTx()
    try {
      val result: Result = graph.execute(query)
      val value = result.columnAs[Long]("count(distinct r)").next()
      transaction.success()
      value > 0
    } finally {
      transaction.close()
    }
  }

  def doesQuestionHaveRelationship(question: Question, relationship: Edge): Boolean = {
    doesNodeHaveRelationship("Question", "question_id", question.question_id.toString, relationship.name())
  }

  def doesTagHaveRelationship(tag: Tag, relationship: Edge): Boolean = {
    doesNodeHaveRelationship("Tag", "name", tag.name, relationship.name())
  }

  /**
   * Method for delete repeated relationship between two nodes. Use it after any downloading process
   */
  def deleteRepeatedRelationship(): Unit = {
    val query =
      """match (a)-[r]-(b)
        |with a, b, TAIL (COLLECT (r)) as rr
        |FOREACH (r IN rr | DELETE r)""".stripMargin
    val transaction: Transaction = graph.beginTx()
    try {
      graph.execute(query)
      transaction.success()
    } finally {
      transaction.close()
    }
  }
}
