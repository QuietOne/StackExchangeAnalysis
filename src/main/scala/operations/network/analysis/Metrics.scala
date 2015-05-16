package operations.network.analysis

import operations.persistance.Neo4j


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
    val intersection = numberOfTagsRelatedTo(tagName1, tagName2)
    val tag1Related = numberOfTagsRelatedTo(tagName1)
    val tag2Related = numberOfTagsRelatedTo(tagName2)
    val all = tag1Related + tag2Related - intersection
    val probabilityIntersection: Double = intersection.toDouble / all
    val probabilityTag1: Double = tag1Related.toDouble / all
    val probabilityTag2: Double = tag2Related.toDouble / all
    math.log(probabilityIntersection / (probabilityTag1 * probabilityTag2)) / math.log(2)
  }
}
