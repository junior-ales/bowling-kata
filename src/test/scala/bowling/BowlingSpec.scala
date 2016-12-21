package bowling

import bowling.Bowling.roll
import bowling.Game.{GameOver, InvalidGame, NotStarted, Started}
import org.scalatest.{FunSpec, Matchers}

class BowlingSpec extends FunSpec with Matchers {

  describe("Bowling") {
    it("not started game") {
      val game = roll(List())

      game shouldBe NotStarted
      game.score shouldBe 0
      game.frames shouldBe empty
    }

    describe("started game") {
      it("type") {
        roll(List(5, 4, 7, 1, 0)) shouldBe a[Started]
        roll(List(0)) shouldBe a[Started]
        roll(List.fill(19)(1)) shouldBe a[Started]

        roll(List.fill(20)(5)) shouldBe a[Started]
        roll(List.fill(21)(5)) shouldNot be(a[Started])

        roll(List.fill(11)(10)) shouldBe a[Started]
        roll(List.fill(12)(10)) shouldNot be(a[Started])
      }

      it("score") {
        roll(List(0)).score shouldBe 0
        roll(List(10)).score shouldBe 10
        roll(List(10, 10, 0, 7)).score shouldBe 44
        roll(List.fill(11)(10)).score shouldBe 290
        roll(List.fill(20)(5)).score shouldBe 145
      }
    }

    describe("game over") {
      it("type") {
        roll(List.fill(12)(10)) shouldBe a[GameOver]
        roll(List.fill(21)(5)) shouldBe a[GameOver]
        roll(List.fill(20)(1)) shouldBe a[GameOver]
      }

      it("should be able to score a game with all zeros") {
        val bowls = List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        roll(bowls).score shouldBe 0
      }
    }

    describe("invalid input") {
      it("should not allow negative numbers") {
        roll(List(-83)) shouldBe a[InvalidGame]
        roll(List(-3, -7)) shouldBe a[InvalidGame]
        roll(List(0, -10, 0)) shouldBe a[InvalidGame]
        roll(List(0, 10, -1, 3)) shouldBe a[InvalidGame]
      }

      it("should not allow numbers more than 10") {
        roll(List(83)) shouldBe a[InvalidGame]
        roll(List(3, 17)) shouldBe a[InvalidGame]
        roll(List(0, 10, 54)) shouldBe a[InvalidGame]
        roll(List(0, 10, 11, 3)) shouldBe a[InvalidGame]
      }
    }

  }

}
