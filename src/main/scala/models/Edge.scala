package models

import org.neo4j.graphdb.RelationshipType

/**
 * All defined edges. Used for easier integration with graph database API.
 */
trait Edge extends RelationshipType {
  override def name() = {getClass.getName}
}

//---------------------------------------------------------------------------
/**
 * Generic edge between two tags.
 */
trait TagToTagEdge extends Edge
/**
 * Edge between two tags defined by StackExchange API as related tags. StackExchange defines related tags as common tag pairings.
 */
case class RelatedTagsEdge() extends TagToTagEdge
/**
 * Edge between two tags defined by StackExchange API as synonym tags. StackExchange defines synonym tags as "specific set of tags".
 * @param to_tag
 * @param from_tag
 * @param applied_count
 */
case class SynonymTagsEdge(val to_tag: String,
                           val from_tag: String,
                           val applied_count: String) extends TagToTagEdge

/**
 * Edge between two similar tags. Tags are similar if tagSimilarity metric is above certain level.
 * @see  Metrics.tagSimilarity
 */
case class SimilarTagsEdge() extends TagToTagEdge

//---------------------------------------------------------------------------
/**
 * Generic edge between tag and question.
 */
trait TagToQuestionEdge extends Edge

/**
 * Edge between tag and question that are related. The question "belongs" to certain tag.
 */
case class BelongsEdge() extends TagToQuestionEdge

/**
 * Edge between tag and question defined by StackExchange API as Frequently Asked Questions. StackExchange defines relation as
 * "frequently asked questions in a set of tags".
 */
case class FAQEdge() extends TagToQuestionEdge

//--------------------------------------------------------------------------
/**
 * Generic edge between two questions.
 */
trait QuestionToQuestionEdge extends Edge

/**
 * Edge between two question defined by StackExchange API as related questions.
 * <p>StackExchange warning:</p>
 * The algorithm for determining if questions are related is not documented, and subject to change at any time. Furthermore,
 * these values are very heavily cached, and may not update immediately after a question has been edited. It is also not
 * guaranteed that a question will be considered related to any number (even non-zero) of questions, and a consumer should be
 * able to handle a variable number of returned questions.
 */
case class RelatedQuestionsEdge() extends QuestionToQuestionEdge

/**
 * Edge between two questions defined by StackExchange API as linked.
 * <p>StackExchange node</p>
 * This method only considers questions that are linked within a site, and will never return questions from another
 * Stack Exchange site. <br>
 * A question is considered "linked" when it explicitly includes a hyperlink to another question, there are no
 * other heuristics.
 */
case class LinkedQuestionsEdge() extends QuestionToQuestionEdge