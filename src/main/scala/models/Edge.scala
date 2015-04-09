package models

import org.neo4j.graphdb.RelationshipType


trait Edge extends RelationshipType {
  override def name() = {getClass.getName}
}

//------------------------------
trait TagToTagEdge extends Edge

case class RelatedTagsEdge() extends TagToTagEdge
case class SynonymTagsEdge(val to_tag: String,
                           val from_tag: String,
                           val applied_count: String) extends TagToTagEdge

//------------------------------
trait TagToQuestionEdge extends Edge

case class BelongsEdge() extends TagToQuestionEdge
case class FAQEdge() extends TagToQuestionEdge

//-------------------------------
trait QuestionToQuestionEdge extends Edge

case class RelatedQuestionsEdge() extends QuestionToQuestionEdge
case class SimilarQuestionsEdge() extends QuestionToQuestionEdge