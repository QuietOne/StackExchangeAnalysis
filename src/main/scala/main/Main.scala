package main

import models.Question
import operations.network.analysis.Metrics
import operations.recommendations.Recommender


object Main extends App {

  def similarityBetweenJavaScalaClojure(): Unit = {
//    println("Downloading data")
//    DownloadingProcedures.downloadDataTags()
    println("Network closures between:")
    print("Java & Scala:    ")
    println(Metrics.networkClosures("java","scala"))
    print("Clojure & Scala: ")
    println(Metrics.networkClosures("clojure","scala"))
    print("Java & Clojure:  ")
    println(Metrics.networkClosures("java","clojure"))
  }

  def recommendMeQuestionWithTag(tagName: String): Unit = {
//    println("Downloading data")
//    DownloadingProcedures.downloadRecommenderData(tagName)
    println("Recommendation for: " + tagName)
    val recommendedQuestion: List[Question] = Recommender.recommendQuestionsForTag(tagName, 5)
    println("Questions can be reached at links:")
    for (question <- recommendedQuestion) {
      println(question.link)
    }
  }
  println("Starting")
  recommendMeQuestionWithTag("c++")
  println("Completed")
}
