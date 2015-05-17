package operations.stack.exchange

import models.{User, Question, SynonymTagsEdge, Tag}
import org.json4s.DefaultFormats
import org.json4s.native.JsonMethods._

import scala.collection.mutable.ListBuffer

/**
 * Object used for parsing data from StackExchange.
 */
object StackExchangeParser {

  /**
   * Extract total parameter in query result
   * @param json
   * @return
   */
  def parseTotal(json: String): Long = {
    implicit val formats = DefaultFormats
    case class Wrapper(val total: Long)

    val jObject = parse(json)
    val wrapper = jObject.extract[Wrapper]
    wrapper.total
  }

  /**
   * Extracting questions from specified json format.
   * @param json
   * @return
   */
  def parseQuestions(json: String): List[Question] = {
    implicit val formats = DefaultFormats
    case class Wrapper(val items: List[Question])

    val jObject = parse(json)
    val wrapper = jObject.extract[Wrapper]
    wrapper.items
  }

  /**
   * Extracting tags from specified json format.
   * @param json
   * @return
   */
  def parseTags(json: String): List[Tag] = {
    implicit val formats = DefaultFormats
    case class Wrapper(val items: List[Tag])

    val jObject = parse(json)
    val wrapper = jObject.extract[Wrapper]
    wrapper.items
  }

  /**
   * Extracting synonym tags object from specified json format.
   * @param json
   * @return
   */
  def parseSynonymTags(json: String): List[SynonymTagsEdge] = {
    implicit  val formats = DefaultFormats
    case class Wrapper(val items: List[SynonymTagsEdge])

    val jObject = parse(json)
    val wrapper = jObject.extract[Wrapper]
    wrapper.items
  }

  def parseUsers(json: String): List[User] = {
    case class TopTag(val user: User)
    implicit  val formats = DefaultFormats
    case class Wrapper(val items: List[TopTag])

    val jObject = parse(json)
    val wrapper = jObject.extract[Wrapper]
    val users = ListBuffer.empty[User]
    for (topTag <- wrapper.items) {
      users += topTag.user
    }
    users.result()
  }

  def parseTopTags(json: String): List[Tag] = {
    case class TopTag(val tag_name: String)
    implicit  val formats = DefaultFormats
    case class Wrapper(val items: List[TopTag])

    val jObject = parse(json)
    val wrapper = jObject.extract[Wrapper]
    val tags = ListBuffer.empty[Tag]
    for (topTag <- wrapper.items) {
      tags += new Tag(topTag.tag_name)
    }
    tags.result()
  }
}
