package bowling

import bowling.Frame.{Last, Strike}
import bowling.Game.{GameOver, NotStarted, Started}
import org.scalatest.{FunSpec, Matchers}

class GameSpec extends FunSpec with Matchers {

  describe("Not Started Game") {
    val game = Game(Right(List()))

    it("should have score 0") {
      game.score shouldBe 0
    }

    it("should have no frames") {
      game.frames shouldBe empty
    }

    describe("type") {
      it("should be NotStarted") {
        game shouldBe NotStarted
      }
    }
  }

  describe("Started Game") {
    describe("first bowl") {
      it("should score the amount of pins knocked down") {
        Game(Right(List(Frame(8)))).score shouldBe 8
        Game(Right(List(Frame(10)))).score shouldBe 10
        Game(Right(List(Frame(0)))).score shouldBe 0
      }
    }

    describe("with no strike or spare") {
      it("should have as score the sum of pins knocked down") {
        Game(Right(List(Frame(0, 9), Frame(1, 0)))).score shouldBe 10
      }
    }

    describe("with spare") {
      it("as last frame") {
        Game(Right(List(Frame(9, 1)))).score shouldBe 10
        Game(Right(List(Frame(0, 9), Frame(9, 1)))).score shouldBe 19
      }

      it("and subsequent frames") {
        val spare = Frame(5, 5)

        Game(Right(List(spare, Frame(2)))).score shouldBe 14
        Game(Right(List(spare, Frame(10)))).score shouldBe 30

        Game(Right(List(spare, Frame(2, 7)))).score shouldBe 21
        Game(Right(List(spare, Frame(0, 0)))).score shouldBe 10
        Game(Right(List(spare, Frame(9, 1)))).score shouldBe 29

        Game(Right(List(spare, spare, spare))).score shouldBe 40
        Game(Right(List(Frame(1, 1), spare, spare))).score shouldBe 27
      }
    }

    describe("with strike") {
      it("as last frame") {
        Game(Right(List(Frame(10)))).score shouldBe 10
        Game(Right(List(Frame(0, 9), Frame(10)))).score shouldBe 19
      }

      it("and subsequent frames") {
        Game(Right(List(Strike, Frame(0)))).score shouldBe 10
        Game(Right(List(Strike, Frame(4)))).score shouldBe 18

        Game(Right(List(Strike, Frame(3, 6)))).score shouldBe 28
        Game(Right(List(Strike, Frame(0, 0)))).score shouldBe 10
        Game(Right(List(Strike, Frame(5, 5)))).score shouldBe 30
        Game(Right(List(Strike, Strike))).score shouldBe 30

        Game(Right(List(Strike, Frame(6, 2), Strike))).score shouldBe 36
        Game(Right(List(Strike, Frame(4, 6), Strike))).score shouldBe 50
        Game(Right(List(Strike, Strike, Frame(8)))).score shouldBe 54
        Game(Right(List(Strike, Strike, Frame(0, 10)))).score shouldBe 50
        Game(Right(List(Strike, Strike, Strike))).score shouldBe 60
      }
    }

    describe("type") {
      val nineFrames = List.fill(9)(Frame(3, 6))

      it("one or more frames should be a Started game") {
        Game(Right(List(Frame(1)))) shouldBe a[Started]
        Game(Right(List(Frame(10), Frame(1)))) shouldBe a[Started]
        Game(Right(List(Frame(2, 6), Frame(10), Frame(1)))) shouldBe a[Started]
      }

      it("up to 9 frames should be a Started game") {
        Game(Right(nineFrames)) shouldBe a[Started]
      }

      it("10 frames after first bowl") {
        Game(Right(nineFrames :+ Last(0))) shouldBe a[Started]
        Game(Right(nineFrames :+ Last(10))) shouldBe a[Started]
        Game(Right(nineFrames :+ Last(9))) shouldBe a[Started]
      }

      it("10 frames with a strike and/or spare on the last one") {
        Game(Right(nineFrames :+ Last(9, Some(1)))) shouldBe a[Started]
        Game(Right(nineFrames :+ Last(0, Some(10)))) shouldBe a[Started]
        Game(Right(nineFrames :+ Last(4, Some(6)))) shouldBe a[Started]
        Game(Right(nineFrames :+ Last(10, Some(10)))) shouldBe a[Started]
        Game(Right(nineFrames :+ Last(10, Some(0)))) shouldBe a[Started]
      }
    }
  }

  describe("Game Over") {

    describe("with no strike or spare") {
      it("should calculate score") {
        Game(Right(List.fill(10)(Frame(0, 0)))).score shouldBe 0
        Game(Right(List.fill(10)(Frame(3, 6)))).score shouldBe 90
      }
    }

    describe("with spare") {
      val nineOpenFrames = List.fill(9)(Frame(3, 6))

      it("before the 10th frame should calculate normally") {
        Game(Right(Frame(5, 5) :: nineOpenFrames)).score shouldBe 94
      }
    }

    describe("with strike") {
      val nineOpenFrames = List.fill(9)(Frame(4, 4))

      it("before the 10th frame should calculate normally") {
        Game(Right(Strike :: nineOpenFrames)).score shouldBe 90
      }
    }

    describe("with fill ball") {
      val nineOpenFrames = List.fill(9)(Frame(4, 5))
      val fillBallSpare = Frame(4, 6, 8)

      val nineStrikes = List.fill(9)(Frame(10))
      val fillBallStrike = Frame(10, 10, 10)

      it("should calculate the fill ball") {
        Game(Right(nineOpenFrames :+ fillBallSpare)).score shouldBe 99
      }

      it("should calculate the perfect game") {
        Game(Right(nineStrikes :+ fillBallStrike)).score shouldBe 300
      }
    }

    describe("type") {
      it("10 frames game should be Game over") {
        val nineFrames = List.fill(9)(Frame(4, 4))

        Game(Right(nineFrames)) shouldNot be(a[GameOver])
        Game(Right(nineFrames :+ Frame(10, 10, 10))) shouldBe a[GameOver]
        Game(Right(nineFrames :+ Frame(10, 10, 0))) shouldBe a[GameOver]
        Game(Right(nineFrames :+ Frame(10, 0, 0))) shouldBe a[GameOver]
        Game(Right(nineFrames :+ Frame(5, 5, 10))) shouldBe a[GameOver]
        Game(Right(nineFrames :+ Frame(0, 10, 10))) shouldBe a[GameOver]
        Game(Right(nineFrames :+ Frame(6, 4, 5))) shouldBe a[GameOver]
        Game(Right(nineFrames :+ Last(5, Some(4)))) shouldBe a[GameOver]
        Game(Right(nineFrames :+ Last(0, Some(0)))) shouldBe a[GameOver]
      }
    }
  }
}
