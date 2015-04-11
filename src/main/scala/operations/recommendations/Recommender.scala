package operations.recommendations

import models.Question
import operations.persistance.Neo4j


object Recommender {

  /*
  Using Katz centrality
   */
  def recommendQuestionsForTag(tagName: String, size: Int = 10, depth: Int = 4): List[Question] = {
    case class Wrapper(val step: Double, val question: Question)
    val alpha: Double = 0.5
    val weightScore: Double = 0.8
    val weightAnswerCount: Double = 0.3
    val weightIsAnswered: Double = 2000
    val weightViewCount: Double = 1.3
    println("Loaded constants")
    Neo4j.openConnection()
    println("Opened connection")
    var mapper: Map[String, Wrapper] = Map()
    //first connected nodes
    var questions: List[Question] = Neo4j.extractFAQOfTag(tagName)
    for (q <- questions) {
      val wrapper: Wrapper = new Wrapper(0, q)
      mapper += q.question_id.toString -> wrapper
    }
    //adding to depth
    for (i <- 1 to depth) {
      questions = Neo4j.extractRelatedQuestions(tagName, i)
      for (q <- questions) {
        if (!mapper.isDefinedAt(q.question_id.toString)) {
          val wrapper: Wrapper = new Wrapper(i, q)
          mapper += q.question_id.toString -> wrapper
        }
      }
    }
    println("Extracted relevant data")
    //calculating values
    var list: List[Wrapper] = List()
    for (w <- mapper) {
      val wrapper = w._2
      val isAnswered = if (wrapper.question.is_answered) 1 else 0
      val calculation = Math.pow(alpha, wrapper.step) *
        ((wrapper.question.answer_count * weightAnswerCount) +
          (isAnswered * weightIsAnswered) +
          (wrapper.question.view_count * weightViewCount) +
          (wrapper.question.score * weightScore))
      list = new Wrapper(calculation, wrapper.question) :: list
    }
    println("Calculated data")
    list.sortBy(_.step).reverse
    Neo4j.closeConnection()
    println("Closed connection")
    var returnee: List[Question] = List()
    for (i <- 0 to size) {
      returnee = list(i).question :: returnee
    }
    returnee.reverse
  }
}
