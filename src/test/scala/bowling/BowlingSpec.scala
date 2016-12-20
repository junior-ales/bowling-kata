package bowling

import org.scalatest.{FunSpec, Matchers}

class BowlingSpec extends FunSpec with Matchers {

  describe("Bowling") {
    it("should calculate a one bowl game") {
      Bowling.roll(List(6)).score shouldBe 6
      Bowling.roll(List(0)).score shouldBe 0
      Bowling.roll(List(10)).score shouldBe 10
    }
  }

}
