package operations.persistance

import models.{RelatedTagsEdge, SynonymTagsEdge, Tag}
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
