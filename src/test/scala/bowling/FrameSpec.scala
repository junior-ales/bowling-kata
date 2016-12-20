package bowling

import bowling.Frame._
import org.scalatest.{FlatSpec, Matchers}

class FrameSpec extends FlatSpec with Matchers {

  "From no bowls" should "return an empty list" in {
    val frames = toFrames(List())
    frames shouldBe empty
  }

  "From 1 bowl - no strike" should "return an ongoing frame" in {
    val frames = toFrames(List(1))
    frames shouldBe List(OnGoing(1))
  }

  "From 1 bowl - strike" should "return a strike frame" in {
    val frames = toFrames(List(10))
    frames shouldBe List(Strike)
  }

  "From 2 bowls - no spare" should "return an open frame" in {
    val frames = toFrames(List(4, 5))
    frames shouldBe List(Open(4, 5))
  }

  "From 2 bowls - gutterball + all" should "return an spare frame" in {
    val frames = toFrames(List(0, 10))
    frames shouldBe List(Spare(0, 10))
  }

  "From 2 bowls - strike + common" should "return 1 strike and 1 ongoing frame" in {
    val frames = toFrames(List(10, 0))
    frames shouldBe List(Strike, OnGoing(0))
  }

  "From 2 bowls - strike + strike" should "return 2 strike frames" in {
    val frames = toFrames(List(10, 10))
    frames shouldBe List(Strike, Strike)
  }

  "From 2 bowls - spare" should "return an spare frame" in {
    val frames = toFrames(List(1, 9))
    frames shouldBe List(Spare(1, 9))
  }

  "From 3 bowls - all common" should "return 1 open and 1 ongoing frame" in {
    val frames = toFrames(List(4, 5, 9))
    frames shouldBe List(Open(4, 5), OnGoing(9))
  }

  "From 3 bowls - spare + common" should "return 1 spare and 1 ongoing frame" in {
    val frames = toFrames(List(1, 9, 8))
    frames shouldBe List(Spare(1, 9), OnGoing(8))
  }

  "From 3 bowls - spare + strike" should "return 1 spare and 1 strike frame" in {
    val frames = toFrames(List(1, 9, 10))
    frames shouldBe List(Spare(1, 9), Strike)
  }

  "From 3 bowls - 3x strike" should "return 3 strike frames" in {
    val frames = toFrames(List(10, 10, 10))
    frames shouldBe List(Strike, Strike, Strike)
  }

  "From 4 bowls - spare + spare" should "return 2 spare frames" in {
    val frames = toFrames(List(1, 9, 8, 2))
    frames shouldBe List(Spare(1, 9), Spare(8, 2))
  }

  "From 6 bowls - spare + spare + spare" should "return 3 spare frames" in {
    val frames = toFrames(List(1, 9, 8, 2, 7, 3))
    frames shouldBe List(Spare(1, 9), Spare(8, 2), Spare(7, 3))
  }

  "Create Frames from bowls" should "create 10 open frames" in {
    toFrames(List.fill(20)(0)) shouldBe List.fill(9)(Frame(0, 0)) :+ Last(0, Some(0))
    toFrames(List(1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8)) shouldBe List.fill(9)(Frame(1, 8)) :+ Last(1, Some(8))
  }

  it should "create a fill ball frame when a spare or strike happens in the second and/or third last bowls" in {
    val bowlsWithFinalStrikes = List(1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 10, 10, 10)
    val expectedFrames = List.fill(9)(Frame(1, 8)) :+ Frame(10, 10, 10)

    toFrames(bowlsWithFinalStrikes) shouldBe expectedFrames
  }

  "Score of NoFrame" should "be always zero" in {
    NoFrame.score() shouldBe 0
  }

  "Score of OnGoing frame" should "be the amount of its pins" in {
    OnGoing(9).score() shouldBe 9
    OnGoing(0).score() shouldBe 0
  }

  "Score of Open frame" should "be the amount of its pins" in {
    Open(0, 0).score(List(Frame(10), Frame(5, 5))) shouldBe 0
    Open(8, 1).score(List(Frame(10), Frame(5, 5))) shouldBe 9
    Open(1, 1).score(List(Frame(0, 10))) shouldBe 2
    Open(1, 1).score(List(Frame(2, 2))) shouldBe 2
    Open(1, 1).score(List(Frame(5, 5, 10))) shouldBe 2
  }

  "Score of Spare frame" should "add the amount of pins of the first subsequent bowl" in {
    // empty and ongoing
    Spare(3, 7).score() shouldBe 10
    Spare(3, 7).score(List(Frame(8))) shouldBe 18
    Spare(3, 7).score(List(Frame(0))) shouldBe 10

    // open
    Spare(3, 7).score(List(Frame(0, 1))) shouldBe 10
    Spare(3, 7).score(List(Frame(9, 0))) shouldBe 19
    Spare(3, 7).score(List(Frame(7, 2))) shouldBe 17

    // spare
    Spare(3, 7).score(List(Frame(0, 10))) shouldBe 10
    Spare(3, 7).score(List(Frame(9, 1))) shouldBe 19

    // strike
    Spare(3, 7).score(List(Frame(10))) shouldBe 20

    // fill ball
    Spare(3, 7).score(List(Frame(10, 10, 10))) shouldBe 20
    Spare(3, 7).score(List(Frame(0, 10, 5))) shouldBe 10
  }

  "Score of Strike frame" should "add the amount of pins of the first and second subsequent bowl" in {
    // empty and ongoing
    Strike.score() shouldBe 10
    Strike.score(List(Frame(0))) shouldBe 10
    Strike.score(List(Frame(1))) shouldBe 11

    // open
    Strike.score(List(Frame(5, 4))) shouldBe 19
    Strike.score(List(Frame(0, 8))) shouldBe 18
    Strike.score(List(Frame(3, 0))) shouldBe 13

    // spare
    Strike.score(List(Frame(0, 10))) shouldBe 20
    Strike.score(List(Frame(9, 1))) shouldBe 20
    Strike.score(List(Frame(5, 5))) shouldBe 20

    // strike
    Strike.score(List(Strike)) shouldBe 20
    Strike.score(List(Strike, Strike)) shouldBe 30
    Strike.score(List(Strike, Frame(1))) shouldBe 21
    Strike.score(List(Strike, Frame(5, 3))) shouldBe 25
    Strike.score(List(Strike, Frame(7, 3))) shouldBe 27
    Strike.score(List(Strike, Frame(7, 3, 5))) shouldBe 27
    Strike.score(List(Strike, Frame(10, 5, 8))) shouldBe 30
    Strike.score(List(Strike, Strike, Strike)) shouldBe 30

    // fill ball
    Strike.score(List(Frame(10, 10, 10))) shouldBe 30
    Strike.score(List(Frame(10, 10, 0))) shouldBe 30
    Strike.score(List(Frame(2, 8, 5))) shouldBe 20
  }

  "Last frame" should "have as score the amount of pins knocked down" in {
    Frame(5, 5, 5).score() shouldBe 15
    Frame(0, 10, 5).score() shouldBe 15
    Frame(10, 5, 5).score() shouldBe 20
    Frame(10, 10, 5).score() shouldBe 25
    Frame(10, 10, 10).score() shouldBe 30

    Last(0).score() shouldBe 0
    Last(10).score() shouldBe 10
    Last(0, Some(0)).score() shouldBe 0
    Last(0, Some(9)).score() shouldBe 9
    Last(10, Some(10)).score() shouldBe 20
  }

  it should "not be finished on the first bowl" in {
    Last(0).finished shouldBe false
    Last(1).finished shouldBe false
    Last(10).finished shouldBe false
    Last(9).finished shouldBe false
  }

  it should "not be finished if a spare and/or strike without fill ball" in {
    Last(10, Some(0)).finished shouldBe false
    Last(0, Some(10)).finished shouldBe false
    Last(3, Some(7)).finished shouldBe false
    Last(10, Some(10)).finished shouldBe false
  }

  it should "be finished if fill ball is rolled" in {
    Last(10, Some(10), Some(10)).finished shouldBe true
    Last(10, Some(10), Some(0)).finished shouldBe true
    Last(0, Some(10), Some(0)).finished shouldBe true
    Last(1, Some(9), Some(9)).finished shouldBe true
  }

  it should "be finished if two bowls done without strike or spare" in {
    Last(0, Some(9)).finished shouldBe true
    Last(9, Some(0)).finished shouldBe true
    Last(0, Some(0)).finished shouldBe true
    Last(1, Some(1)).finished shouldBe true
  }
}
