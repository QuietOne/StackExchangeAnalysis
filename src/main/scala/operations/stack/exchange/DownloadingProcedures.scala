package operations.stack.exchange

import models._
import operations.network.analysis.Metrics
import operations.persistance.Neo4j

import scala.collection.mutable.ListBuffer

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
   * Downloading procedure for similarity metrics.
   * @param tagNames
   * @see Metrics.tagSimilarity
   * @see Metrics.pointMutualInformation
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

  def forPMIMetrics(tagNames: List[String]): Unit = {
    println("Started downloading data for PMI metrics for tags: " + tagNames)
    val tags: ListBuffer[Tag] = ListBuffer.empty
    for (tagName <- tagNames) {
      tags += new Tag(tagName)
    }
    Neo4j.persistTags(tags.result())
    println("Tags persisted")
    println("Started downloading askers")
    for (tag <- tagNames) {
      if (Neo4j.extractTopAskers(tag) == List()) {
        val askers: List[User] = StackExchangeAPIExtractor.extractTopAskers(tag)
        Neo4j.persistTopAskers(askers, tag)
        println("\t" + tag + " top askers persisted")
        for (asker <- askers) {
          Neo4j.persistTopTags(StackExchangeAPIExtractor.extractTopTagsByUser(asker.user_id), asker)
          println("\t\tPersisted top tags for user " + asker.user_id)
        }
      }
    }
    println("Top Askers persisted.")
  }

  /**
   * Downloading procedure for recommender data.
   * @param tagName
   */
  def downloadRecommenderData(tagName: String): Unit = {
    Neo4j.persistTag(new Tag(tagName))
    println("Tag " + tagName + " persisted")
    if (Neo4j.extractSynonymTagsIncludingMe(tagName).length <= 1) {
      val synonyms: List[SynonymTagsEdge] =
        StackExchangeAPIExtractor.extractTagsSynonyms(tagName, page = 1, pageSize = 100)
      Neo4j.persistSynonymTags(synonyms)
      println("Synonyms for tag " + tagName + " persisted")
    } else {
      println("Synonyms for tag " + tagName + " are already in database")
    }
    val allSynonymTags: List[Tag] = Neo4j.extractSynonymTagsIncludingMe(tagName)
    for (tag <- allSynonymTags) {
      println("Starting download process for tag: " + tag.name)
      if (!Neo4j.doesTagHaveRelationship(tag, new FAQEdge)) {
        val questions: List[Question] = StackExchangeAPIExtractor.extractFAQ(tag.name, page = 1, pageSize = 100)
        Neo4j.persistFAQ(tag.name, questions)
        println("\tFAQ persisted")
      } else {
        println("\tFAQ is already in database")
      }
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
        DownloadingProcedures.forPMIMetrics(tagNames)
        for (iTag <- tagNames) {
          if (tagName.ne(iTag) && !tagName.equals(iTag)) {
            println("\t\t\tCalculating metrics for " + tagName + " & " + iTag)
            val pmi = Metrics.pointMutualInformation(tagName, iTag)
            Neo4j.persistPMI(tagName, iTag, pmi)
            if (!Neo4j.doesTagHaveRelationship(Tag(iTag), new FAQEdge)) {
              val iQuestions: List[Question] = StackExchangeAPIExtractor.extractFAQ(iTag)
              Neo4j.persistFAQ(iTag, iQuestions)
              println("\t\t\tFAQ for " + iTag + " persisted")
            } else {
              println("\t\t\tFAQ for " + iTag + " are already in database")
            }
            val iMonteCarlo: List[Question] = StackExchangeAPIExtractor.extractBelongsQuestions(iTag)
            Neo4j.persistQuestions(iMonteCarlo)
            println("\t\t\tDownloading data for " + iTag + " completed")
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
