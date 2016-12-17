package bowling

import bowling.Frame.Strike
import bowling.Game.{NotStarted, Started}
import org.scalatest.{FunSpec, Matchers}

class GameSpec extends FunSpec with Matchers {

  describe("Not Started Game") {
    val game = Game(List())
    game shouldBe NotStarted

    it("should have score 0") {
      game.score shouldBe 0
    }
    it("should have no frames") {
      game.frames shouldBe empty
    }
  }

  describe("Started Game") {
    describe("first bowl") {
      it("should score the amount of pins knocked down") {
        val game = Game(List(Frame(8)))

        game shouldBe a[Started]
        game.score shouldBe 8
      }
    }

    describe("with no strike or spare") {
      it("should have as score the sum of pins knocked down") {
        Game(List(Frame(0, 9), Frame(1, 0))).score shouldBe 10
      }

      it("up to 9 frames should be a started game") {
        val openFrame = Frame(3, 6)
        val game = Game(List(openFrame, openFrame, openFrame, openFrame, openFrame, openFrame, openFrame, openFrame, openFrame))

        game shouldBe a[Started]
        game.score shouldBe 81
      }
    }

    describe("with spare") {
      describe("as last frame") {
        it("should score as the number of all pins knocked down") {
          Game(List(Frame(9, 1))).score shouldBe 10
          Game(List(Frame(0, 9), Frame(9, 1))).score shouldBe 19
        }
      }

      describe("and subsequent frames") {
        it("should double the amount of pins of the FIRST subsequent bowl") {
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
    }

    describe("with strike") {
      describe("as last frame") {
        it("should score as the number of all pins knocked down") {
          Game(List(Frame(10))).score shouldBe 10
          Game(List(Frame(0, 9), Frame(10))).score shouldBe 19
        }
      }

      describe("and subsequent frames") {
        it("should double the amount of pins of the FIRST and SECOND subsequent bowls") {
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
    }
  }
}
