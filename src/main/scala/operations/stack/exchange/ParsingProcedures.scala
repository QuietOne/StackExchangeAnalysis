package operations.stack.exchange

import models.{Question, SynonymTagsEdge, Tag}
import org.json4s.DefaultFormats
import org.json4s.native.JsonMethods._

/**
 * Object used for parsing data from StackExchange.
 */
object StackExchangeParser {
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
}
