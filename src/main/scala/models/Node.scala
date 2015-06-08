package models

/**
 * Trait for all nodes used for persisting. Not needed for now, as there are no additional requirement set by Neo4j.
 */
trait Node

/**
 * Question
 * @param question_id unique id given by StackExchange API
 * @param link link to the site
 * @param tags tags to which this question belongs
 * @param view_count count view
 * @param favorite_count
 * @param up_vote_count
 * @param down_vote_count
 * @param answer_count
 * @param score
 * @param is_answered
 */
case class Question(val question_id: Int,
                    val link: String,
                    val tags: List[String] = null,
                    val view_count: Double = 0,
                    val favorite_count: Double = 0,
                    val up_vote_count: Double = 0,
                    val down_vote_count: Double = 0,
                    val answer_count: Double = 0,
                    val score: Double = 0,
                    val is_answered: Boolean = false,
                    val last_activity_date: Double = 0,
                    val creation_date: Double = 0) extends Node

/**
 * Tag
 * @param name unique name given by StackExchange API
 */
case class Tag(val name: String) extends Node

/**
 * User
 * @param user_id unique id given by StackExchange API
 */
case class User(val user_id: Long) extends Node