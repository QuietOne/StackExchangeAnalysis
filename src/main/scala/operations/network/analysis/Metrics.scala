package operations.network.analysis

import operations.persistance.Neo4j


object Metrics {
  /*
  Network Closure: A measure of the completeness of relational triads.
  An individual’s assumption of network closure (i.e. that their friends are also friends)
  is called transitivity. Transitivity is an outcome of the individual or situational trait
  of Need for Cognitive Closure.

  In this example this metrics will be used to identify how close are two tags
   */
  def networkClosures(tagName1: String, tagName2: String): Double = {
    Neo4j.openConnection()
    val query1 =
      """match (t1:Tag)-[:`models.RelatedTagsEdge`]-(n:Tag)-[:`models.RelatedTagsEdge`]-(t2:Tag)
        |where t1.name="""".stripMargin + tagName1 + """" and t2.name="""" + tagName2 + """"
        |return count(distinct n)""".stripMargin
    var similarCount: Long = Neo4j.executeNetworkClosureQuery(query1)
    val query2 =
      """match (t1:Tag)-[:`models.SynonymTagsEdge`]-(:Tag)-[:`models.RelatedTagsEdge`]-(n:Tag)-[:`models.RelatedTagsEdge`]-(t2:Tag)
        |where t1.name="""".stripMargin + tagName1 + """" and t2.name="""" + tagName2 + """"
        |return count(distinct n)""".stripMargin
    similarCount += Neo4j.executeNetworkClosureQuery(query2)
    val query3 =
      """match (t1:Tag)-[:`models.RelatedTagsEdge`]-(n:Tag)-[:`models.RelatedTagsEdge`]-(:Tag)-[:`models.SynonymTagsEdge`]-(t2:Tag)
        |where t1.name="""".stripMargin + tagName1 + """" and t2.name="""" + tagName2 + """"
        |return count(distinct n)""".stripMargin
    similarCount += Neo4j.executeNetworkClosureQuery(query3)
    val query4 =
      """match (t1:Tag)-[:`models.SynonymTagsEdge`]-(:Tag)-[:`models.RelatedTagsEdge`]-(n:Tag)-[:`models.RelatedTagsEdge`]-(:Tag)-[:`models.SynonymTagsEdge`]-(t2:Tag)
        |where t1.name="""".stripMargin + tagName1 + """" and t2.name="""" + tagName2 + """"
        |return count(distinct n)""".stripMargin
    similarCount += Neo4j.executeNetworkClosureQuery(query4)
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
    val networkClosurePercent: Double = similarCount.toDouble / (sum - similarCount) * 100

//    println("similar" + similarCount)
//    println("sum" + sum)
    Neo4j.closeConnection()
    networkClosurePercent
  }
}
