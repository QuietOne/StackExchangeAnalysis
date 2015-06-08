package operations.network.analysis

import models.Tag
import operations.persistance.Neo4j

import scala.collection.mutable.ListBuffer


object Metrics {

  private def numberOfTagsRelatedTo(tagName1: String, tagName2: String): Long = {
    val query1 =
      """match (t1:Tag)-[:`models.RelatedTagsEdge`]-(n:Tag)-[:`models.RelatedTagsEdge`]-(t2:Tag)
        |where t1.name="""".stripMargin + tagName1 + """" and t2.name="""" + tagName2 + """"
        |return count(distinct n)""".stripMargin
    val query2 =
      """match (t1:Tag)-[:`models.SynonymTagsEdge`]-(:Tag)-[:`models.RelatedTagsEdge`]-(n:Tag)-[:`models.RelatedTagsEdge`]-(t2:Tag)
        |where t1.name="""".stripMargin + tagName1 + """" and t2.name="""" + tagName2 + """"
        |return count(distinct n)""".stripMargin
    val query3 =
      """match (t1:Tag)-[:`models.RelatedTagsEdge`]-(n:Tag)-[:`models.RelatedTagsEdge`]-(:Tag)-[:`models.SynonymTagsEdge`]-(t2:Tag)
        |where t1.name="""".stripMargin + tagName1 + """" and t2.name="""" + tagName2 + """"
        |return count(distinct n)""".stripMargin
    val query4 =
      """match (t1:Tag)-[:`models.SynonymTagsEdge`]-(:Tag)-[:`models.RelatedTagsEdge`]-(n:Tag)-[:`models.RelatedTagsEdge`]-(:Tag)-[:`models.SynonymTagsEdge`]-(t2:Tag)
        |where t1.name="""".stripMargin + tagName1 + """" and t2.name="""" + tagName2 + """"
        |return count(distinct n)""".stripMargin
    Neo4j.executeCountQuery(query1) + Neo4j.executeCountQuery(query2) + Neo4j.executeCountQuery(query3) + Neo4j.executeCountQuery(query4)
  }

  private def numberOfTagsRelatedTo(tagName: String): Long = {
    val query =
      """match (t:Tag)-[:`models.RelatedTagsEdge`]-(n:Tag)
        |where t.name="""".stripMargin + tagName + """"
        |return n
        |UNION
        |match (t:Tag)-[:`models.SynonymTagsEdge`]-()-[:`models.RelatedTagsEdge`]-(n:Tag)
        |where t.name="""".stripMargin + tagName + """"
        |return n""".stripMargin
    Neo4j.executeCountOfUnion(query)
  }
  /**
   * Method for calculating similarity between two tags.
   * @param tagName1
   * @param tagName2
   * @return number from 0 to 100, where 100 means they very similar, and 0 not at all.
   */
  def tagSimilarity(tagName1: String, tagName2: String): Double = {
    val similarCount = numberOfTagsRelatedTo(tagName1, tagName2)
    val sum = numberOfTagsRelatedTo(tagName1) + numberOfTagsRelatedTo(tagName2)
    similarCount.toDouble / (sum - similarCount) * 100
  }

  /**
   * Method for calculating similarity between two tags.
   * @param tagName1
   * @param tagName2
   * @return
   */
  def pointMutualInformation(tagName1: String, tagName2: String): Double = {
    val topAskersForTag1 = Neo4j.extractTopAskers(tagName1)
    val topAskersForTag2 = Neo4j.extractTopAskers(tagName2)

    val topTagsOfTopAskersForTag1 = ListBuffer.empty[Tag]
    for (asker <- topAskersForTag1) {
      topTagsOfTopAskersForTag1 ++= Neo4j.extractTopTags(asker.user_id.toString)
    }
    val topTagsOfTopAskersForTag2 = ListBuffer.empty[Tag]
    for (asker <- topAskersForTag2) {
      topTagsOfTopAskersForTag2 ++= Neo4j.extractTopTags(asker.user_id.toString)
    }

    val tag1Related = topTagsOfTopAskersForTag1.distinct.length
    println("\tTag1Related:  " + tag1Related)
    val tag2Related = topTagsOfTopAskersForTag2.distinct.length
    println("\tTag2Related:  " + tag2Related)
    val intersection = topTagsOfTopAskersForTag1.distinct.intersect(topTagsOfTopAskersForTag2.distinct).length
    println("\tIntersection: " + intersection)
    val all = (topTagsOfTopAskersForTag1 ++ topTagsOfTopAskersForTag2).distinct.length
    println("\tAll:          " + all)

    val probabilityIntersection: Double = intersection.toDouble / all
    println("\tPIntersection:" + probabilityIntersection)
    val probabilityTag1: Double = tag1Related.toDouble / all
    println("\tPTag1:        " + probabilityTag1)
    val probabilityTag2: Double = tag2Related.toDouble / all
    println("\tPTag2:        " + probabilityTag2)
    val pmi = math.log(probabilityIntersection / (probabilityTag1 * probabilityTag2)) / math.log(2)
    if (pmi == 0) 0 else - pmi / (math.log(probabilityIntersection) / math.log(2))
  }
}
