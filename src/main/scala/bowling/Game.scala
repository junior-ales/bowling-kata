package bowling

import bowling.Frame.{OnGoing, Open, Spare, Strike}

sealed trait Game {
  type BowlingResult = List[(Frame, Int)]

  def spareSumWith(f: Option[Frame]): Int = f match {
    case None => 0
    case Some(Strike) => Strike.pins
    case Some(Spare(bowl1, _)) => bowl1
    case Some(Open(bowl1, _)) => bowl1
    case Some(OnGoing(bowl1)) => bowl1
  }

  def strikeSumWith(frames: List[Frame]): Int = frames match {
    case List() => 0
    case f :: Nil => f.pins
    case Open(bowl1, bowl2) :: _ => bowl1 + bowl2
    case Spare(bowl1, bowl2) :: _ => bowl1 + bowl2

    case Strike :: OnGoing(bowl) :: Nil => Strike.pins + bowl
    case Strike :: Spare(bowl1, _) :: _ => Strike.pins + bowl1
    case Strike :: Open(bowl1, _) :: _ => Strike.pins + bowl1
    case Strike :: Strike :: _ => Strike.pins * 2

    case OnGoing(_) :: _ => throw new Error("forbidden ongoing + other frame")
    case Strike :: OnGoing(bowl) :: _ => throw new Error("forbidden ongoing + other frame")
  }

  private def firstFrameSum(frames: List[Frame]): Int = frames match {
    case List() => 0
    case Strike :: fsTail => Strike.pins + strikeSumWith(fsTail)
    case Spare(bowl1, bowl2) :: fsTail => bowl1 + bowl2 + spareSumWith(fsTail.headOption)
    case OnGoing(bowl) :: Nil => bowl
    case OnGoing(_) :: _ => throw new Error("forbidden ongoing + other frame")
    case f :: _ => f.pins
  }

  def result: BowlingResult = {
    def calcResult(fs: List[Frame], res: BowlingResult): BowlingResult = {
      if (fs.isEmpty) res
      else calcResult(fs.tail, (fs.head, firstFrameSum(fs)) :: res)
    }

    calcResult(this.frames, List())
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
