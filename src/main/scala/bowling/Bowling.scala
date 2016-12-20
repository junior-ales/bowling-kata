package bowling

import bowling.Frame.toFrames

object Bowling {
  def roll(bowls: List[Int]): Game = Game(toFrames(bowls))
}
