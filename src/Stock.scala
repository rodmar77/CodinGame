import java.util.Scanner
import scala.annotation.tailrec

object Stock extends App {

  val in = new Scanner(System.in);
  val map = Range(0, in.nextInt).map(n => in.nextInt).zipWithIndex

  val min = getMin(map)

  def getMin(map: Seq[(Int, Int)]): Int = {
    def getMin(index: Int, acc: Int): Int = {
      if (index >= map.size) (acc)
      else {
        val low = lowestIndexAfter(index)
        val high = highestIndexBetween(index, low)
        getMin(low + index, Math.min(min, map(low)._1 - map(high)._1))
      }
    }

    def highestIndexBetween(low: Int, high: Int): Int = {
      map
        .slice(low, high)
        .sortWith(_._1 > _._1)
        .last
        ._2
    }

    def lowestIndexAfter(index: Int): Int = {
      map
        .slice(index, map.size)
        .sortWith(_._1 < _._1)
        .head
        ._2
    }
    
    getMin(0, 0)
  }
}