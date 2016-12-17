package bowling

import bowling.Frame.Strike
import bowling.Game.{GameOver, NotStarted, Started}
import org.scalatest.{FunSpec, Matchers}

class GameSpec extends FunSpec with Matchers {

  describe("Not Started Game") {
    val game = Game(List())

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
        Game(List(Frame(8))).score shouldBe 8
        Game(List(Frame(10))).score shouldBe 10
        Game(List(Frame(0))).score shouldBe 0
      }
    }

    describe("with no strike or spare") {
      it("should have as score the sum of pins knocked down") {
        Game(List(Frame(0, 9), Frame(1, 0))).score shouldBe 10
      }
    }

    describe("with spare") {
      it("as last frame") {
        Game(List(Frame(9, 1))).score shouldBe 10
        Game(List(Frame(0, 9), Frame(9, 1))).score shouldBe 19
      }

      it("and subsequent frames") {
        val spare = Frame(5, 5)

        Game(List(spare, Frame(2))).score shouldBe 14
        Game(List(spare, Frame(10))).score shouldBe 30

        Game(List(spare, Frame(2, 7))).score shouldBe 21
        Game(List(spare, Frame(0, 0))).score shouldBe 10
        Game(List(spare, Frame(9, 1))).score shouldBe 29

        Game(List(spare, spare, spare)).score shouldBe 40
        Game(List(Frame(1, 1), spare, spare)).score shouldBe 27
      }
    }

    describe("with strike") {
      it("as last frame") {
        Game(List(Frame(10))).score shouldBe 10
        Game(List(Frame(0, 9), Frame(10))).score shouldBe 19
      }

      it("and subsequent frames") {
        Game(List(Strike, Frame(0))).score shouldBe 10
        Game(List(Strike, Frame(4))).score shouldBe 18

        Game(List(Strike, Frame(3, 6))).score shouldBe 28
        Game(List(Strike, Frame(0, 0))).score shouldBe 10
        Game(List(Strike, Frame(5, 5))).score shouldBe 30
        Game(List(Strike, Strike)).score shouldBe 30

        Game(List(Strike, Frame(6, 2), Strike)).score shouldBe 36
        Game(List(Strike, Frame(4, 6), Strike)).score shouldBe 50
        Game(List(Strike, Strike, Frame(8))).score shouldBe 54
        Game(List(Strike, Strike, Frame(0, 10))).score shouldBe 50
        Game(List(Strike, Strike, Frame(7, 1))).score shouldBe 53
        Game(List(Strike, Strike, Strike)).score shouldBe 60
      }
    }

    describe("type") {
      it("one or more frames should be a Started game") {
        Game(List()) shouldNot be(a[Started])

        Game(List(Frame(1))) shouldBe a[Started]
        Game(List(Frame(10), Frame(1))) shouldBe a[Started]
        Game(List(Frame(2, 6), Frame(10), Frame(1))) shouldBe a[Started]
      }

      it("up to 9 frames should be a Started game") {
        Game(List.fill(9)(Frame(3, 6))) shouldBe a[Started]
        Game(List.fill(10)(Frame(3, 6))) shouldNot be(a[Started])
      }
    }
  }

  describe("Game Over") {

    describe("with no strike or spare") {
      it("should calculate score") {
        Game(List.fill(10)(Frame(0, 0))).score shouldBe 0
        Game(List.fill(10)(Frame(3, 6))).score shouldBe 90
      }
    }

    describe("with spare") {
      val nineOpenFrames = List.fill(9)(Frame(3, 6))

      it("before the 10th frame should calculate normally") {
        Game(Frame(5, 5) :: nineOpenFrames).score shouldBe 94
      }
    }

    describe("with strike") {
      val nineOpenFrames = List.fill(9)(Frame(4, 4))

      it("before the 10th frame should calculate normally") {
        Game(Strike :: nineOpenFrames).score shouldBe 90
      }
    }

    describe("type") {
      it("10 frames game should be Game over") {
        Game(List.fill(10)(Frame(4, 4))) shouldBe a[GameOver]
        Game(List.fill(9)(Frame(4, 4))) shouldNot be(a[GameOver])
        Game(List.fill(11)(Frame(4, 4))) shouldNot be(a[GameOver])
      }
    }
  }
}
