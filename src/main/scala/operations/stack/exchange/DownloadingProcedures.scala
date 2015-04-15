package operations.stack.exchange

import models.{Question, SynonymTagsEdge, Tag}
import operations.network.analysis.Metrics
import operations.persistance.Neo4j

/**
 * Object with methods for downloading (getting data from StackExchange API and persisting it to Neo4j) for further
 * analysis. There are different downloading procedures depending on type of analysis, that should be performed on.<br>
 * There a lot of messages for console while downloading data, as this process can be very slow, and some output while
 * downloading data is user-friendly.
 */
object DownloadingProcedures {

  /**
   * Method for starting download process.
   */
  def startDownloadingProcess(): Unit = {
    println("Starting downloading process")
    Neo4j.openConnection()
    println("Opened connection")
  }
  /**
   * Method for finishing download.
   */
  def finishDownloadingProcess(): Unit = {
    println("Started cleaning downloaded data.")
    Neo4j.deleteRepeatedRelationship()
    println("Finished cleaning download data.")
    Neo4j.closeConnection()
    println("Closed connection")
    println("Downloading process successfully finished.")
  }

  /**
   * Downloading procedure for tagSimilarity metrics.
   * @param tagNames
   * @see Metrics.tagSimilarity
   */
  def forTagSimilarityMetrics(tagNames: List[String]): Unit = {
    println("Started downloading data for tag similarity for tags: " + tagNames)
    var tags: List[Tag] = List()
    for (tagName <- tagNames) {
      tags = new Tag(tagName) :: tags
    }
    Neo4j.persistTags(tags)
    println("Tags persisted")
    //persist tags synonyms to inputed tags
    println("Started downloading synonyms for given tags.")
    for (tag <- tags) {
      val synonyms: List[SynonymTagsEdge] =
        StackExchangeAPIExtractor.extractTagsSynonyms(tag.name, page = 1, pageSize = 100)
      Neo4j.persistSynonymTags(synonyms)
      println("\t" + tag.name + " synonyms persisted.")
    }
    println("Synonyms tags persisted.")
    //get related tags to all tags persisted in database
    println("Started downloading related tags data.")
    var allTags: List[Tag] = List()
    for (tagName <- tagNames) {
      allTags = Neo4j.extractSynonymTagsIncludingMe(tagName) ++ allTags
    }
    for (tag <- allTags) {
      val relatedTags: List[Tag] =
        StackExchangeAPIExtractor.extractTagsRelated(tag.name, page = 1, pageSize = 100)
      Neo4j.persistRelatedTags(tag.name, relatedTags)
      println("\t" + tag.name + " related tags persisted.")
    }
    println("Related tags persisted")
  }

  /**
   * Downloading procedure for recommender data.
   * @param tagName
   */
  def downloadRecommenderData(tagName: String): Unit = {
    Neo4j.persistTag(new Tag(tagName))
    println("Tag " + tagName + " persisted")
    val synonyms: List[SynonymTagsEdge] =
      StackExchangeAPIExtractor.extractTagsSynonyms(tagName, page = 1, pageSize = 100)
    Neo4j.persistSynonymTags(synonyms)
    println("Synonyms for tag " + tagName + " persisted")
    val allSynonymTags: List[Tag] = Neo4j.extractSynonymTagsIncludingMe(tagName)
    for (tag <- allSynonymTags) {
      println("Starting download process for tag: " + tag.name)
      val questions: List[Question] = StackExchangeAPIExtractor.extractFAQ(tag.name, page = 1, pageSize = 100)
      Neo4j.persistFAQ(tag.name, questions)
      println("\tFAQ persisted")
      val monteCarloQuestions: List[Question] =
        StackExchangeAPIExtractor.extractBelongsQuestions(tag.name, page = 1, pageSize = 100)
      Neo4j.persistQuestions(monteCarloQuestions)
      println("\tMonte Carlo questions persisted")
      println("\tExtracting related data for MonteCarlo questions")
      for (question <- monteCarloQuestions) {
        println("\t\tExtracting data for question: " + question.link)
        val tags: List[Tag] = Neo4j.extractBelongsTags(question.question_id.toString)
        //extract names from given tags
        var tagNames: List[String] = List()
        for (iTag <- tags) tagNames = iTag.name :: tagNames
        println("\t\tStart downloading data for related tags")
        DownloadingProcedures.forTagSimilarityMetrics(tagNames)
        for (iTag <- tagNames) {
          if (tagName.ne(iTag)) {
            println("\t\t\tCalculating metrics for " + tagName + " & " + iTag)
            if (Metrics.tagSimilarity(tagName, iTag) > 60) {
              Neo4j.persistSimilarTagsEdge(tagName, iTag)
              println("\t\t\tSimilar; Start downloading extra data for " + iTag)
              val iQuestions: List[Question] = StackExchangeAPIExtractor.extractFAQ(iTag)
              Neo4j.persistFAQ(iTag, iQuestions)
              println("\t\t\tFAQ for " + iTag + " persisted")
              val iMonteCarlo: List[Question] = StackExchangeAPIExtractor.extractBelongsQuestions(iTag)
              Neo4j.persistQuestions(iMonteCarlo)
              println("\t\t\tDownloading data for " + iTag + " completed")
            } else {
              println("\t\t\tNot Similar;")
            }
          }
        }
      }
    }
    println("Downloading bunch of related questions")
    val questions: List[Question] = Neo4j.extractQuestions()
    for (question <- questions) {
      val relatedQuestions: List[Question] =
        StackExchangeAPIExtractor.extractQuestionsRelated(question.question_id, page = 1, pageSize = 100)
      Neo4j.persistRelatedQuestions(question, relatedQuestions)
    }
    println("Related questions persisted")
  }
}
