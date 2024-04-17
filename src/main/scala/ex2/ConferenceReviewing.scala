package ex2

import ex2.Question.{RELEVANCE, SIGNIFICANCE}

enum Question:
  case CONFIDENCE
  case FINAL
  case RELEVANCE
  case SIGNIFICANCE

trait ConferenceReviewing:
  def loadReview(article: Int, scores: Map[Question, Int]): Unit
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit
  def orderedScores(article: Int, question: Question): List[Int]
  def averageFinalScore(article: Int): Double
  def acceptedArticles(): Set[Integer]
  def sortedAcceptedArticles(): List[(Int, Double)]
  def averageWeightedFinalScoreMap(): Map[Int, Double]

object ConferenceReviewing:
  def apply(): ConferenceReviewing = ConferenceReviewingImpl()

  private class ConferenceReviewingImpl() extends ConferenceReviewing:
    import Question.*
    private case class Review(relevance: Int, significance: Int, confidence: Int, fin: Int):
      def vote(q: Question): Int = q match
        case RELEVANCE => relevance
        case SIGNIFICANCE => significance
        case CONFIDENCE => confidence
        case FINAL => fin

    private var reviews: List[(Int, Review)] = List()

    override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
      loadReview(article, scores.getOrElse(RELEVANCE, 0), scores.getOrElse(SIGNIFICANCE, 0), scores.getOrElse(CONFIDENCE, 0), scores.getOrElse(FINAL, 0))

    override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
      reviews = reviews.+:(article, Review(relevance, significance, confidence, fin))


    override def orderedScores(article: Int, question: Question): List[Int] =
      reviews.filter((id, _) => id == article).map((_, rew) => rew.vote(question)).toList.sorted

    override def averageFinalScore(article: Int): Double =
      reviews.collect { case (id, rew) if id == article => rew.vote(FINAL)}.sum.toDouble / reviews.count((i, _) => i == article).toDouble

    override def acceptedArticles(): Set[Integer] = ???

    override def sortedAcceptedArticles(): List[(Int, Double)] = ???

    override def averageWeightedFinalScoreMap(): Map[Int, Double] = ???
