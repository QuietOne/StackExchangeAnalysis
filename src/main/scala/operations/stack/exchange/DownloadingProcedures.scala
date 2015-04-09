package operations.stack.exchange

import models.{SynonymTagsEdge, Tag}
import operations.persistance.Neo4j

object DownloadingProcedures {

   //Data for question: What is more popular Java, Clojure or Scala?
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
}
