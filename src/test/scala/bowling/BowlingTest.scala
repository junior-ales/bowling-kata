package bowling

import org.scalatest.{FunSpec, Matchers}

class BowlingTest extends FunSpec with Matchers {

  describe("Bowling") {
    it("should calculate a one bowl game") {
      Bowling.roll(List(6)).score shouldBe 6
    }
  }

}
