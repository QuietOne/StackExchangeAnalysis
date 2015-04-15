package operations.recommendations

import models.Question
import operations.persistance.Neo4j


object Recommender {

  /**
   * Method for recommending question for the specific tag.
   * @param tagName the specified tag
   * @param size size of questions returned
   * @param depth depth of searching questions. It is not recommended to go above depth 3, as the complexity of algorithm
   *              is exponential.
   * @return list of questions
   */
  def recommendQuestionsForTag(tagName: String, size: Int = 10, depth: Int = 3): List[Question] = {
    case class Wrapper(val value: Double, val question: Question)
    val alpha: Double = 0.05
    val weightScore: Double = 0.8
    val weightAnswerCount: Double = 0.3
    val weightIsAnswered: Double = 2000
    val weightViewCount: Double = 0.
    val weightBelongs: Double = 1000000
    println("Loaded constants")
    Neo4j.openConnection()
    println("Opened connection")
    var mapper: Map[String, Wrapper] = Map()
    //adding to depth
    for (i <- 0 to depth) {
      val questions: List[Question] = Neo4j.extractQuestionsAtDepth(tagName, i)
      for (q <- questions) {
        if (!mapper.isDefinedAt(q.question_id.toString)) {
          val wrapper: Wrapper = new Wrapper(i, q)
          mapper += q.question_id.toString -> wrapper
        }
      }
      println("Added questions from " + i + ". depth")
    }
    println("Extracted relevant data")
    //calculating values
    var list: List[Wrapper] = List()
    for (w <- mapper) {
      val wrapper = w._2
      val isAnswered = if (wrapper.question.is_answered) 1 else 0
      val belongs = if (Neo4j.ifQuestionBelongToTag(wrapper.question.question_id.toString, tagName)) 1 else 0
      val calculation = Math.pow(alpha, wrapper.value) *
        ((wrapper.question.answer_count * weightAnswerCount) +
          (isAnswered * weightIsAnswered) +
          (wrapper.question.view_count * weightViewCount) +
          (wrapper.question.score * weightScore) +
          (belongs * weightBelongs))
      list = new Wrapper(calculation, wrapper.question) :: list
    }
    println("Calculated data")
    list = list.sortWith(_.value > _.value)
    Neo4j.closeConnection()
    println("Closed connection")
    var returnee: List[Question] = List()
    for (i <- 0 to size) {
      returnee = list(i).question :: returnee
    }
    returnee
  }
}
