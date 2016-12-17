package bowling

sealed trait Frame {
  def pins: Int
}

object Frame {
  final case class OnGoing(bowl: Int) extends Frame {
    override def pins: Int = bowl
  }

  final case class Open(bowl1: Int, bowl2: Int) extends Frame {
    override def pins: Int = bowl1 + bowl2
  }

  final case class Spare(bowl1: Int, bowl2: Int) extends Frame {
    override def pins: Int = 10
  }

  case object Strike extends Frame {
    override def pins: Int = 10
  }

  def apply(bowl: Int): Frame =
    if (bowl == 10) Strike else OnGoing(bowl)

  def apply(bowl1: Int, bowl2: Int): Frame = Frame(OnGoing(bowl1), bowl2)

  def apply(frame: OnGoing, bowl: Int): Frame = frame match {
    case OnGoing(score) if score + bowl == 10 => Spare(score, bowl)
    case OnGoing(score) if score + bowl < 10 => Open(score, bowl)
    //case _ if bowl == 10 => throw new Error("forbidden ongoing + 10")
  }

  private def mergeFrames(frame: Frame, bowl: Int): List[Frame] = frame match {
    case f: OnGoing => List(Frame(f, bowl))
    case _ => List(frame, Frame(bowl))
  }

  def toFrames(bowls: List[Int]): List[Frame] = {
    def _toFrames(_bowls: List[Int], acc: List[Frame]): List[Frame] = _bowls match {
      case Nil => acc
      case b :: bs => _toFrames(bs, acc.init ::: mergeFrames(acc.last, b))
    }

    bowls match {
      case Nil => Nil
      case b :: bs => _toFrames(bs, List(Frame(b)))
    }
  }
}
