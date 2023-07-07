
case class Move(movingMarbles: Map[Pos, Pos]) {
  require(moveDirectionOpt.isDefined, s"Invalid Move, undefined direction: $movingMarbles")
  require(movingMarbles.toVector.length <= 5, s"Invalid Move, too many marbles: $movingMarbles")
  require(movingMarbles.values.count(_ == Pos.OFF) <= 1, s"Invalid Move, too much death: $movingMarbles")

  private lazy val moveDirectionOpt: Option[MoveDirection] = {
    val before = movingMarbles.keys.toVector
    val after = movingMarbles.values.toVector.map(Some(_))

    if (after == before.map(_.upLeft)) Some(UpLeft)
    else if (after == before.map(_.upRight)) Some(UpRight)
    else if (after == before.map(_.left)) Some(Left)
    else if (after == before.map(_.right)) Some(Right)
    else if (after == before.map(_.downLeft)) Some(DownLeft)
    else if (after == before.map(_.downRight)) Some(DownRight)
    else None
  }

  def moveDirection: MoveDirection = moveDirectionOpt.get

  def killed: Option[Pos] = movingMarbles.find { case (_, p) => p == Pos.OFF }.map(_._1)
  def isKill: Boolean = killed.nonEmpty
}
