package operations.stack.exchange

import models.{Question, SynonymTagsEdge, Tag}
import operations.persistance.Neo4j

object DownloadingProcedures {

   //Data for question: How similar are Java, Clojure or Scala?
  def downloadDataTags(): Unit = {
     Neo4j.openConnection()
     println("Opened connection")
     //persist Java, Clojure and Scala tag
     val tags: List[Tag] = List(new Tag("java"), new Tag("scala"), new Tag("clojure"))
     Neo4j.persistTags(tags)
     println("Main tags persisted")
     //persist tags synonyms to java, clojure, and scala
     for (tag <- tags) {
       val synonyms: List[SynonymTagsEdge] =
         StackExchangeAPIExtractor.extractTagsSynonyms(tag.name, page = 1, pageSize = 100)
       Neo4j.persistSynonymTags(synonyms)
       println(tag.name + " synonyms persisted.")
     }
     println("Synonyms tags persisted")
     //get related tags to all tags persisted in database
     val allTags: List[Tag] = Neo4j.extractTags
     for (tag <- allTags) {
       var relatedTags: List[Tag] =
         StackExchangeAPIExtractor.extractTagsRelated(tag.name, page = 1, pageSize = 100)
       Neo4j.persistRelatedTags(tag.name, relatedTags)
       relatedTags = StackExchangeAPIExtractor.extractTagsRelated(tag.name, page = 2, pageSize = 100)
       Neo4j.persistRelatedTags(tag.name, relatedTags)
       println(tag.name + " related tags persisted.")
     }
     println("Related tags persisted")
     Neo4j.closeConnection()
     println("Closed connections")
   }

  def downloadRecommenderData(tagName: String): Unit = {
    Neo4j.openConnection()
    println("Opened connection")
//    Neo4j.persistTag(new Tag(tagName))
//    println("Tag " + tagName + " persisted")
//    val synonyms: List[SynonymTagsEdge] =
//      StackExchangeAPIExtractor.extractTagsSynonyms(tagName, page = 1, pageSize = 100)
//    Neo4j.persistSynonymTags(synonyms)
    println("Synonyms for tag " + tagName + " persisted")
    val allSynonymTags: List[Tag] = Neo4j.extractSynonymTagsIncludingMe(tagName)
    for (tag <- allSynonymTags) {
      val questions: List[Question] = StackExchangeAPIExtractor.extractFAQ(tag.name, page = 1, pageSize = 100)
      Neo4j.persistFAQ(tag.name, questions)
      println("FAQ for " + tag.name + " persisted")
      for (question <- questions) {
        val relatedQuestions: List[Question] =
          StackExchangeAPIExtractor.extractQuestionsRelated(question.question_id, page = 1, pageSize = 100)
        Neo4j.persistRelatedQuestions(question, relatedQuestions)
      }
      println("Related questions persisted")
    }
    Neo4j.closeConnection()
    println("Closed connection")
  }
}
