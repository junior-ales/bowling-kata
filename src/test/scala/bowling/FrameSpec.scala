package bowling

import bowling.Frame._
import org.scalatest.{FlatSpec, Matchers}

class FrameSpec extends FlatSpec with Matchers {

  "From no bowls" should "return an empty list" in {
    val frames = Frame.toFrames(List())
    frames shouldBe empty
  }

  "From 1 bowl - no strike" should "return an ongoing frame" in {
    val frames = Frame.toFrames(List(1))
    frames shouldBe List(OnGoing(1))
  }

  "From 1 bowl - strike" should "return a strike frame" in {
    val frames = Frame.toFrames(List(10))
    frames shouldBe List(Strike)
  }

  "From 2 bowls - no spare" should "return an open frame" in {
    val frames = Frame.toFrames(List(4, 5))
    frames shouldBe List(Open(4, 5))
  }

  "From 2 bowls - gutterball + all" should "return an spare frame" in {
    val frames = Frame.toFrames(List(0, 10))
    frames shouldBe List(Spare(0, 10))
  }

  "From 2 bowls - strike + common" should "return 1 strike and 1 ongoing frame" in {
    val frames = Frame.toFrames(List(10, 0))
    frames shouldBe List(Strike, OnGoing(0))
  }

  "From 2 bowls - strike + strike" should "return 2 strike frames" in {
    val frames = Frame.toFrames(List(10, 10))
    frames shouldBe List(Strike, Strike)
  }

  "From 2 bowls - spare" should "return an spare frame" in {
    val frames = Frame.toFrames(List(1, 9))
    frames shouldBe List(Spare(1, 9))
  }

  "From 3 bowls - all common" should "return 1 open and 1 ongoing frame" in {
    val frames = Frame.toFrames(List(4, 5, 9))
    frames shouldBe List(Open(4, 5), OnGoing(9))
  }

  "From 3 bowls - spare + common" should "return 1 spare and 1 ongoing frame" in {
    val frames = Frame.toFrames(List(1, 9, 8))
    frames shouldBe List(Spare(1, 9), OnGoing(8))
  }

  "From 3 bowls - spare + strike" should "return 1 spare and 1 strike frame" in {
    val frames = Frame.toFrames(List(1, 9, 10))
    frames shouldBe List(Spare(1, 9), Strike)
  }

  "From 3 bowls - 3x strike" should "return 3 strike frames" in {
    val frames = Frame.toFrames(List(10, 10, 10))
    frames shouldBe List(Strike, Strike, Strike)
  }

  "From 4 bowls - spare + spare" should "return 2 spare frames" in {
    val frames = Frame.toFrames(List(1, 9, 8, 2))
    frames shouldBe List(Spare(1, 9), Spare(8, 2))
  }

  "From 6 bowls - spare + spare + spare" should "return 3 spare frames" in {
    val frames = Frame.toFrames(List(1, 9, 8, 2, 7, 3))
    frames shouldBe List(Spare(1, 9), Spare(8, 2), Spare(7, 3))
  }

  "Score of NoFrame" should "be always zero" in {
    NoFrame.score(List()) shouldBe 0
  }

  it should "not allow other frames to be summed up" in {
    intercept[Error] {
      NoFrame.score(List(Frame(1)))
    }
  }

  "Score of OnGoing frame" should "be the amount of its pins" in {
    OnGoing(9).score(List()) shouldBe 9
    OnGoing(0).score(List()) shouldBe 0
  }

  it should "not allow other frames to be summed up" in {
    intercept[Error] {
      OnGoing(1).score(List(Frame(1)))
    }
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
    Spare(3, 7).score(List()) shouldBe 10
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
    Strike.score(List()) shouldBe 10
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

  "Score of FillBall Frame" should "return the amount of pins knocked down" in {
    FillBall(5, 5, 5).score(List()) shouldBe 15
    FillBall(0, 10, 5).score(List()) shouldBe 15
    FillBall(10, 5, 5).score(List()) shouldBe 20
    FillBall(10, 10, 5).score(List()) shouldBe 25
    FillBall(10, 10, 10).score(List()) shouldBe 30
  }

  it should "not allow other frames to sum up" in {
    intercept[Error] {
      FillBall(5, 5, 5).score(List(Frame(3)))
    }
  }
}
