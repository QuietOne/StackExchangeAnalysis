package main

import operations.network.analysis.Metrics


object Main extends App {
  println("Starting")
//  DownloadingProcedures.downloadDataTags()
  println("Network closures between:")
  print("Java & Scala:    ")
  println(Metrics.networkClosures("java","scala"))
  print("Clojure & Scala: ")
  println(Metrics.networkClosures("clojure","scala"))
  print("Java & Clojure:  ")
  println(Metrics.networkClosures("java","clojure"))
  println("Completed")
}
