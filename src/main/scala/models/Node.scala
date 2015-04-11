package models

trait Node

case class Question(val question_id: Int,
                    val link: String,
                    val tags: List[String] = null,
                    val view_count: Int = 0,
                    val favorite_count: Int = 0,
                    val up_vote_count: Int = 0,
                    val down_vote_count: Int = 0,
                    val answer_count: Int = 0,
                    val score: Int = 0,
                    val is_answered: Boolean = false) extends Node

case class Tag(val name: String) extends Node