package operations.network.analysis

import operations.persistance.Neo4j


object Metrics {

  /**
   * Method for calculating similarity between two tags.
   * @param tagName1
   * @param tagName2
   * @return number from 0 to 100, where 100 means they very similar, and 0 not at all.
   */
  def tagSimilarity(tagName1: String, tagName2: String): Double = {
    val query1 =
      """match (t1:Tag)-[:`models.RelatedTagsEdge`]-(n:Tag)-[:`models.RelatedTagsEdge`]-(t2:Tag)
        |where t1.name="""".stripMargin + tagName1 + """" and t2.name="""" + tagName2 + """"
        |return count(distinct n)""".stripMargin
    var similarCount: Long = Neo4j.executeCountQuery(query1)
    val query2 =
      """match (t1:Tag)-[:`models.SynonymTagsEdge`]-(:Tag)-[:`models.RelatedTagsEdge`]-(n:Tag)-[:`models.RelatedTagsEdge`]-(t2:Tag)
        |where t1.name="""".stripMargin + tagName1 + """" and t2.name="""" + tagName2 + """"
        |return count(distinct n)""".stripMargin
    similarCount += Neo4j.executeCountQuery(query2)
    val query3 =
      """match (t1:Tag)-[:`models.RelatedTagsEdge`]-(n:Tag)-[:`models.RelatedTagsEdge`]-(:Tag)-[:`models.SynonymTagsEdge`]-(t2:Tag)
        |where t1.name="""".stripMargin + tagName1 + """" and t2.name="""" + tagName2 + """"
        |return count(distinct n)""".stripMargin
    similarCount += Neo4j.executeCountQuery(query3)
    val query4 =
      """match (t1:Tag)-[:`models.SynonymTagsEdge`]-(:Tag)-[:`models.RelatedTagsEdge`]-(n:Tag)-[:`models.RelatedTagsEdge`]-(:Tag)-[:`models.SynonymTagsEdge`]-(t2:Tag)
        |where t1.name="""".stripMargin + tagName1 + """" and t2.name="""" + tagName2 + """"
        |return count(distinct n)""".stripMargin
    similarCount += Neo4j.executeCountQuery(query4)
    val query5 =
      """match (t:Tag)-[:`models.RelatedTagsEdge`]-(n:Tag)
        |where t.name="""".stripMargin + tagName1 + """"
        |return n
        |UNION
        |match (t:Tag)-[:`models.SynonymTagsEdge`]-()-[:`models.RelatedTagsEdge`]-(n:Tag)
        |where t.name="""".stripMargin + tagName1 + """"
        |return n""".stripMargin
    var sum: Long = Neo4j.executeCountOfUnion(query5)
    val query6 =
      """match (t:Tag)-[:`models.RelatedTagsEdge`]-(n:Tag)
        |where t.name="""".stripMargin + tagName2 + """"
        |return n
        |UNION
        |match (t:Tag)-[:`models.SynonymTagsEdge`]-()-[:`models.RelatedTagsEdge`]-(n:Tag)
        |where t.name="""".stripMargin + tagName2 + """"
        |return n""".stripMargin

    sum += Neo4j.executeCountOfUnion(query6)

    //similar has been subtracted from sum because it is twice included (once with query5 and once with query6)
    similarCount.toDouble / (sum - similarCount) * 100
  }
}
