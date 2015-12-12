import scala.io.StdIn._
import java.util.Scanner

object Heat extends App {

  val in = new Scanner(System.in)
  val (w, h, n, x0, y0) = (in.nextInt, in.nextInt, in.nextInt, in.nextInt, in.nextInt)
  printSequence(Building(w, h), n, x0, y0, in)

  def printSequence(building: Building, rounds: Int, x0: Int, y0: Int, in: Scanner): Unit = {
    def printSequence(current: Building, round: Int): Unit = {
      if (round < rounds) {
        val (x, y) = current.jump
        println(x + " " + y)
        printSequence(current.reduce(in.next, x, y), round + 1)
      }
    }

    printSequence(building.reduce(in.next, x0, y0), 1)
  }
}

object Building {
  def apply(maxX: Int, maxY: Int, minX: Int = 0, minY: Int = 0) = new Building(maxX, maxY, minX, minY)
}

class Building(val maxX: Int, val maxY: Int, val minX: Int = 0, val minY: Int = 0) {
  def reduce(direction: String, x: Int, y: Int) = direction match {
    case "U" => Building(x, y, x, minY)
    case "D" => Building(x, maxY, x, y)
    case "R" => Building(maxX, y, x, y)
    case "L" => Building(x, y, minX, y)
    case "UR" => Building(maxX, y, x, minY)
    case "UL" => Building(x, y, minX, minY)
    case "DR" => Building(maxX, maxY, x, y)
    case default => Building(x, maxY, minX, y)
  }

  def jump: (Int, Int) = ((maxX + minX) / 2, (maxY + minY) / 2)
}