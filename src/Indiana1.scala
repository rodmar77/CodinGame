import java.util.Scanner

object Side {
  def valueOf(side: String) = side match {
    case "LEFT" => LEFT
    case "RIGHT" => RIGHT
    case "TOP" => TOP
    case default => BOTTOM
  }
}

case class Side(val name: String) {
  def opposite = this match {
    case LEFT => RIGHT
    case RIGHT => LEFT
    case BOTTOM => TOP
    case default => BOTTOM
  }
}

object LEFT extends Side("LEFT")
object RIGHT extends Side("RIGHT")
object TOP extends Side("TOP")
object BOTTOM extends Side("BOTTOM")

object Piece {
  def valueOf(piece: Int) = piece match {
    case 0 => _0; case 1 => _1; case 2 => _2;
    case 3 => _3; case 4 => _4; case 5 => _5;
    case 6 => _6; case 7 => _7; case 8 => _8;
    case 9 => _9; case 10 => _10; case 11 => _11;
    case 12 => _12; case default => _13;
  }
}

case class Piece(val name: String, val entries: (Side, Side)*) {
  def canGoTo(otherPiece: Piece, thisEntry: Side, sideWays: Boolean) = {
    entries
      .filter(_._1 == thisEntry)
      .map(n => (n._2, otherPiece
        .entries
        .filter(k =>
          (n._2 == k._1.opposite) &&
            ((n._2 == BOTTOM) ^ (sideWays)))))
      .find(n => !n._2.isEmpty)
      .isDefined
  }

  def getEntryFrom(otherPiece: Piece, otherEntry: Side) = {
    otherPiece
      .entries
      .filter(_._1 == otherEntry)
      .flatMap(n => entries.filter(k => k._1 == n._2.opposite))
      .head
      ._1
  }
}

object _0 extends Piece("0")
object _1 extends Piece("1", (TOP, BOTTOM), (LEFT, BOTTOM), (RIGHT, BOTTOM))
object _2 extends Piece("2", (LEFT, RIGHT), (RIGHT, LEFT))
object _3 extends Piece("3", (TOP, BOTTOM))
object _4 extends Piece("4", (TOP, LEFT), (RIGHT, BOTTOM))
object _5 extends Piece("5", (TOP, RIGHT), (LEFT, BOTTOM))
object _6 extends Piece("6", (LEFT, RIGHT), (RIGHT, LEFT))
object _7 extends Piece("7", (TOP, BOTTOM), (RIGHT, BOTTOM))
object _8 extends Piece("8", (LEFT, BOTTOM), (RIGHT, BOTTOM))
object _9 extends Piece("9", (TOP, BOTTOM), (LEFT, BOTTOM))
object _10 extends Piece("10", (TOP, LEFT))
object _11 extends Piece("11", (TOP, RIGHT))
object _12 extends Piece("12", (RIGHT, BOTTOM))
object _13 extends Piece("13", (LEFT, BOTTOM))

object Indiana1 extends App {
  val in = new Scanner(System.in);
  val (w, h) = (in.nextInt, in.nextInt)

  val maze = Array.tabulate(h, w)((_, _) => Piece.valueOf(in.nextInt))
  val (exit, enX, enY) = (in.nextInt, in.nextInt, in.nextInt)
  val enSide = Side.valueOf(in.next)

  val moves = getMazeMoves(maze, enX, enY, enSide, exit).mkString("\n")

  def getMazeMoves(maze: Array[Array[Piece]], enX: Int, enY: Int, enSide: Side, exit: Int) = {
    def move(currPiece: Piece, currEntry: Side, x: Int, y: Int, sideWays: Boolean, acc: List[String]) = {
      val (nextPiece, nextMove) = (maze(y)(x), x + " " + y)
      if ((!acc.contains(nextMove)) && (currPiece.canGoTo(nextPiece, currEntry, sideWays)))
        (getMazeMoves(x, y, nextPiece.getEntryFrom(currPiece, currEntry), acc :+ nextMove))
      else (List[String]())
    }

    def getMazeMoves(currX: Int, currY: Int, currEntry: Side, acc: List[String]): List[String] = {
      if ((currY == maze.length - 1) && (exit == currX)) (acc)
      else List((currX - 1, currY, true), (currX + 1, currY, true), (currX, currY + 1, false))
        .filter(p => (maze.isDefinedAt(p._2) && maze(p._2).isDefinedAt(p._1)))
        .map(p => move(maze(currY)(currX), currEntry, p._1, p._2, p._3, acc))
        .filterNot(_.isEmpty)
        .headOption
        .getOrElse(List[String]())
    }

    getMazeMoves(enX, enY, enSide, List[String]())
  }

}