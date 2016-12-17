package bowling

sealed trait Game {
  type BowlingResult = List[(Frame, Int)]

  def result: BowlingResult = {
    def calcBowlingResult(_fs: List[Frame], res: BowlingResult): BowlingResult = _fs match {
      case Nil => res
      case f :: fs => calcBowlingResult(fs, (f, f.score(fs)) :: res)
    }

    calcBowlingResult(this.frames, List())
  }

  def frames: List[Frame]

  def score: Int = result.foldLeft(0)((acc, result) => acc + result._2)
}

object Game {

  final case class Started(frames: List[Frame]) extends Game

  final case class GameOver(frames: List[Frame]) extends Game

  case object NotStarted extends Game {
    override def frames = List()
  }

  def apply(frames: List[Frame]): Game = frames.length match {
    case 0 => NotStarted
    case 10 => GameOver(frames)
    case _ => Started(frames)
  }
}
