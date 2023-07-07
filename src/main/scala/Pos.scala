
// --------X-----------------------X-------------------------X
// INDEX System(Main System)
// 56 57 58 59 60
// 50 51 52 53 54 55
// 43 44 45 46 47 48 49
// 35 36 37 38 39 40 41 42
// 26 27 28 29 30 31 32 33 34
// 18 19 20 21 22 23 24 25
// 11 12 13 14 15 16 17
// 5 6 7 8 9 10
// 0 1 2 3 4

// ----------X--------------X------------X---------X----------
// NormalCoordinateAxis
// (8,0) ,(8,1) ,(8,2) ,(8,3) ,(8,4)
// (7,0) (7,1) ,(7,2) ,(7,3) ,(7,4) ,(7,5)
// (6,0) (6,1) (6,2) ,(6,3) ,(6,4) ,(6,5) (6,6)
// (5,0) (5,1) (5,2) ,(5,3) ,(5,4) ,(5,5) (5,6), (5,7)
// (4,0) (4,1) (4,2) ,(4,3) ,(4,4) ,(4,5) (4,6), (4,7),(4,8)
// (3,0) (3,1) (3,2) ,(3,3) ,(3,4) ,(3,5) (3,6), (3,7)
// (2,0) (2,1) (2,2) ,(2,3) ,(2,4) ,(2,5) , (2,6)
// (1,0) (1,1) ,(1,2) ,(1,3) ,(1,4) (1,5)
// (0,0) ,(0,1) ,(0,2) ,(0,3) (0,4)
//
// --------------X--------------------------X--------------------X------
case class Pos(index: Int) {
  require(index >= -1 && index <= 60, s"Invalid index: $index")

  lazy val (x: Int, y: Int) = {
    if (index == -1) (-1, -1)
    else {
      val (rowStart, x) = Pos.rowStartIndices.zipWithIndex.findLast(_._1 <= index).get
      (x, index - rowStart)
    }

    /* For tests later
    if (index < 5) (0, index)
    else if (index < 11) (1, index - 5)
    else if (index < 18) (2, index - 11)
    else if (index < 26) (3, index - 18)
    else if (index < 35) (4, index - 26)
    else if (index < 43) (5, index - 35)
    else if (index < 50) (6, index - 43)
    else if (index < 56) (7, index - 50)
    else (8, index - 56)
     */
  }

  lazy val rowName: Char = ('A'.toInt + x).toChar
  lazy val colName: Char = ('1'.toInt + y).toChar
  lazy val name: String = {
    if (index == -1) "OFF"
    else new String(Array(rowName, colName))
  }

  private def noneIfOff(newPos: => Option[Pos]): Option[Pos] = {
    if (index == -1) None
    else newPos
  }

  def left: Option[Pos] = noneIfOff(Pos.at(x, y - 1))
  def right: Option[Pos] = noneIfOff(Pos.at(x, y + 1))
  def upLeft: Option[Pos] = noneIfOff {
    if (x < 4) Pos.at(x + 1, y)
    else Pos.at(x + 1, y - 1)
  }
  def upRight: Option[Pos] = noneIfOff {
    if (x < 4) Pos.at(x + 1, y + 1)
    else Pos.at(x + 1, y)
  }
  def downLeft: Option[Pos] = noneIfOff {
    if (x < 4) Pos.at(x - 1, y - 1)
    else Pos.at(x - 1, y)
  }
  def downRight: Option[Pos] = noneIfOff {
    if (x < 4) Pos.at(x - 1, y)
    else Pos.at(x - 1, y + 1)
  }
  def inDirection(moveDirection: MoveDirection): Option[Pos] = {
    moveDirection match {
      case UpLeft => upLeft
      case UpRight => upRight
      case Left => left
      case Right => right
      case DownLeft => downLeft
      case DownRight => downRight
    }
  }
}

object Pos {

  val rowStartIndices: Vector[Int] = Vector(0, 5, 11, 18, 26, 35, 43, 50, 56)

  def apply(x: Int, y: Int): Pos = {
    Pos(rowStartIndices(x) + y)
  }

  def at(x: Int, y: Int): Option[Pos] = {
    if (x >= 0 && x <= 8) {
      val yMax = if (x < 8) rowStartIndices(x+1) - rowStartIndices(x) else 5
      if (y >= 0 && y < yMax) Some(Pos(x, y))
      else if (y == -1 || y == yMax) Some(OFF)
      else None
    } else if ((x == -1 || x == 9) && (y >= -1 && y <= 4)) {
      Some(OFF)
    } else None
  }

  val OFF: Pos = Pos(-1)
  val A1: Pos = Pos(0)
  val A2: Pos = Pos(1)
  val A3: Pos = Pos(2)
  val A4: Pos = Pos(3)
  val A5: Pos = Pos(4)
  val B1: Pos = Pos(5)
  val B2: Pos = Pos(6)
  val B3: Pos = Pos(7)
  val B4: Pos = Pos(8)
  val B5: Pos = Pos(9)
  val B6: Pos = Pos(10)
  val C1: Pos = Pos(11)
  val C2: Pos = Pos(12)
  val C3: Pos = Pos(13)
  val C4: Pos = Pos(14)
  val C5: Pos = Pos(15)
  val C6: Pos = Pos(16)
  val C7: Pos = Pos(17)
  val D1: Pos = Pos(18)
  val D2: Pos = Pos(19)
  val D3: Pos = Pos(20)
  val D4: Pos = Pos(21)
  val D5: Pos = Pos(22)
  val D6: Pos = Pos(23)
  val D7: Pos = Pos(24)
  val D8: Pos = Pos(25)
  val E1: Pos = Pos(26)
  val E2: Pos = Pos(27)
  val E3: Pos = Pos(28)
  val E4: Pos = Pos(29)
  val E5: Pos = Pos(30)
  val E6: Pos = Pos(31)
  val E7: Pos = Pos(32)
  val E8: Pos = Pos(33)
  val E9: Pos = Pos(34)
  val F1: Pos = Pos(35)
  val F2: Pos = Pos(36)
  val F3: Pos = Pos(37)
  val F4: Pos = Pos(38)
  val F5: Pos = Pos(39)
  val F6: Pos = Pos(40)
  val F7: Pos = Pos(41)
  val F8: Pos = Pos(42)
  val G1: Pos = Pos(43)
  val G2: Pos = Pos(44)
  val G3: Pos = Pos(45)
  val G4: Pos = Pos(46)
  val G5: Pos = Pos(47)
  val G6: Pos = Pos(48)
  val G7: Pos = Pos(49)
  val H1: Pos = Pos(50)
  val H2: Pos = Pos(51)
  val H3: Pos = Pos(52)
  val H4: Pos = Pos(53)
  val H5: Pos = Pos(54)
  val H6: Pos = Pos(55)
  val I1: Pos = Pos(56)
  val I2: Pos = Pos(57)
  val I3: Pos = Pos(58)
  val I4: Pos = Pos(59)
  val I5: Pos = Pos(60)

  val all: Vector[Pos] = (-1).to(60).toVector.map(Pos.apply)
}
