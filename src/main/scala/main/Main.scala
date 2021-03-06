package main

import models.Question
import operations.network.analysis.Metrics
import operations.persistance.Neo4j
import operations.recommendations.Recommender
import operations.stack.exchange.DownloadingProcedures


object Main extends App {

  /**
   * Example function for showing tag similarity between Java, Scala and Clojure
   */
  def similarityBetweenJavaScalaClojure(): Unit = {
    val java = "java"
    val scala = "scala"
    val clojure = "clojure"
    DownloadingProcedures.startDownloadingProcess()
    DownloadingProcedures.forTagSimilarityMetrics(List(java, clojure, scala))
    DownloadingProcedures.finishDownloadingProcess()
    Neo4j.openConnection()
    println("Tag similarity between:")
    print("Java & Scala:     ")
    println(Metrics.tagSimilarity(java, scala))
    print("Clojure & Scala:  ")
    println(Metrics.tagSimilarity(clojure, scala))
    print("Java & Clojure:   ")
    println(Metrics.tagSimilarity(java, clojure))
    Neo4j.closeConnection()
  }

  def similarityBetweenJavaScalaClojureUsingPMI(): Unit = {
    val java = "java"
    val scala = "scala"
    val clojure = "clojure"
    DownloadingProcedures.startDownloadingProcess()
    DownloadingProcedures.forPMIMetrics(List(java, clojure, scala))
    DownloadingProcedures.finishDownloadingProcess()
    Neo4j.openConnection()
    println("Point Mutual Information:")
    print("Java & Scala:     ")
    println(Metrics.pointMutualInformation(java, scala))
    print("Clojure & Scala:  ")
    println(Metrics.pointMutualInformation(clojure, scala))
    print("Java & Clojure:   ")
    println(Metrics.pointMutualInformation(java, clojure))
    Neo4j.closeConnection()
  }

  /**
   * Example function for showing interesting "random" questions to specific
   * @param tagName - name of tag for which you want interesting question. Check if there is tag with that name in
   *                StackExchange before searching it
   */
  def recommendMeQuestionForTag(tagName: String): Unit = {
//    DownloadingProcedures.startDownloadingProcess()
//    try {
//      DownloadingProcedures.downloadRecommenderData(tagName)
//    } catch {
//      case e: Exception => e.printStackTrace()
//    }
//    DownloadingProcedures.finishDownloadingProcess()
    println("Recommendation for: " + tagName)
    val recommendedQuestions: List[Question] = Recommender.recommendQuestionsForTag(tagName, size = 10, depth = 3)
    println("Questions can be reached at links:")
    for (question <- recommendedQuestions) {
      println(question.link)
    }
  }

  //main program
  println("Starting")
//  recommendMeQuestionForTag("artificial-intelligence")
  recommendMeQuestionForTag("game-ai")
  println("Completed")

}
