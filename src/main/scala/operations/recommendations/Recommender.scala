package operations.recommendations

import models.Question
import operations.persistance.Neo4j

import scala.collection.mutable.ListBuffer


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
    val weightIsAnswered: Double = 1
    val weightViewCount: Double = 0.8
    val weightBelongs: Double = 1
    val weightCreationDate: Double = 0.03
    val weightLastActivityDate: Double = 0.03
    val declusteringBonus: Double = 1
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
      println("Added " + questions.length + " questions from " + i + ". depth")
    }
    println("Extracted relevant data")
    println("Normalization started")
    val maxScore = mapper.valuesIterator.maxBy(_.question.score).question.score
    val maxAnswerCount = mapper.valuesIterator.maxBy(_.question.answer_count).question.answer_count
    val maxViewCount = mapper.valuesIterator.maxBy(_.question.view_count).question.view_count
    val maxCreationDate = mapper.valuesIterator.maxBy(_.question.creation_date).question.creation_date
    val maxLastActivityDate = mapper.valuesIterator.maxBy(_.question.last_activity_date).question.last_activity_date
    val normalizedList = ListBuffer.empty[Wrapper]
    for (w <- mapper) {
      val wrapper = w._2
      val question = wrapper.question
      normalizedList += wrapper.copy(
        value = wrapper.value / depth,
        question = question.copy(
          score = question.score / maxScore,
          answer_count = question.answer_count / maxAnswerCount,
          view_count = question.view_count / maxViewCount,
          creation_date = question.creation_date / maxCreationDate,
          last_activity_date = question.last_activity_date / maxLastActivityDate
        ))
    }
    println("Normalization ended")
    //calculating values
    var list: List[Wrapper] = List()
    for (wrapper <- normalizedList.result()) {
      val isAnswered = if (wrapper.question.is_answered) 1 else 0
      val belongs = if (Neo4j.ifQuestionBelongToTag(wrapper.question.question_id.toString, tagName)) 1 else 0
      val calculation = Math.pow(alpha, wrapper.value) *
        ((wrapper.question.answer_count * weightAnswerCount) +
          (isAnswered * weightIsAnswered) +
          (wrapper.question.view_count * weightViewCount) +
          (wrapper.question.score * weightScore) +
          (wrapper.question.creation_date * weightCreationDate) +
          (wrapper.question.last_activity_date * weightLastActivityDate) +
          (belongs * weightBelongs))
      list = new Wrapper(calculation, wrapper.question) :: list
    }
    println("Calculated data for " + list.length + " questions.")
    case class Cluster(val name: String, val value: Double)
    val averagePMIValue = Neo4j.averagePMIValue
    val tags = Neo4j.extractClusterForTag(tagName)
    val clusteringList: ListBuffer[Cluster] = ListBuffer.empty
    for (tag <- tags) {
      var edgeTotal: Double = 0
      var edgeCount: Double = 0
      for (tag2 <- tags) {
        val pmi = Neo4j.extractPMI(tag.name, tagName)
        if (pmi!= null && pmi.weight > averagePMIValue) edgeTotal+=1
        edgeCount+=1
      }
      clusteringList += new Cluster(tag.name, edgeTotal / edgeCount)
      println("\tCluster " + tag.name + " added.")
    }
    println("Calculated values of clusters")
    val desclusteringList: ListBuffer[Wrapper] = ListBuffer.empty
    for (wrapper <- list) {
      var bonus: Double = 0
      for (cluster <- clusteringList) {
        if (Neo4j.ifQuestionIsInRelationshipWithTag(wrapper.question.question_id.toString, cluster.name)) {
          bonus += cluster.value
        }
      }
      desclusteringList += wrapper.copy(value = wrapper.value + bonus * declusteringBonus)
      if (bonus >0) {
        println("\tDecluster bonus for question " + wrapper.question.link + "added.")
      }
    }
    list = desclusteringList.result()
    println("Added declustering bonus")
    list = list.sortWith(_.value > _.value)
    Neo4j.closeConnection()
    println("Closed connection")
    var returnee: List[Question] = List()
    for (i <- 0 to size) {
      returnee = list(i).question :: returnee
//      println(list(i).value + " " + list(i).question.link)
    }
    returnee
  }
}
