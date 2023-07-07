
sealed trait MoveDirection {
  def opposite: MoveDirection = {
    this match {
      case UpLeft => DownRight
      case UpRight => DownLeft
      case Left => Right
      case Right => Left
      case DownLeft => UpRight
      case DownRight => UpLeft
    }
  }
}

case object UpLeft extends MoveDirection
case object UpRight extends MoveDirection
case object Left extends MoveDirection
case object Right extends MoveDirection
case object DownLeft extends MoveDirection
case object DownRight extends MoveDirection

object MoveDirection {
  def all: Vector[MoveDirection] = Vector(UpLeft, UpRight, Left, Right, DownLeft, DownRight)

  def relativeDirection(pos1: Pos, pos2: Pos): Option[MoveDirection] = {
    all.find(direction => pos1.inDirection(direction).contains(pos2))
  }
}