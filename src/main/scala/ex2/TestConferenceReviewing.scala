package ex2

import java.util.EnumMap

class TestConferenceReviewing:

  private var cl = ConferenceReviewing()

  private def init(): Unit =
    cr.loadReview(1, 8, 8, 6, 8)
    cr.loadReview(1, 9, 9, 6, 9)
    cr.loadReview(2, 9, 9, 10, 9)
    cr.loadReview(2, 4, 6, 10, 6)
    cr.loadReview(3, 3, 3, 3, 3)
    cr.loadReview(3, 4, 4, 4, 4)
    cr.loadReview(4, 6, 6, 6, 6)
    cr.loadReview(4, 7, 7, 8, 7)
    val map = Map.apply((Question.RELEVANCE, 8), (Question.SIGNIFICANCE, 8), (Question.CONFIDENCE, 7), (Question.FINAL, 8))
    cr.loadReview(4, map)
    cr.loadReview(5, 6, 6, 6, 10)
    cr.loadReview(5, 7, 7, 7, 10)

  @Test def testOrderedScores: Unit =
    init()
    assertEquals(cd.orderedScores(2, Question.RELEVANCE), List.apply(4,9))
    assertEquals(cd.orderedScores(4, Question.CONFIDENCE), List.apply(6, 7, 8))
    assertEquals(cd.orderedScores(5, Question.FINAL), List.apply(10, 10))

