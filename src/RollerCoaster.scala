import java.util.Scanner
import java.util.Deque

object RollerCoaster extends App {
  val in = new Scanner(System.in)
  val (seats, times, groups) = (in.nextInt, in.nextInt, in.nextInt)
  val queue = Range(0, groups).map(n => in.nextInt).reverse

  println(getTotal(seats, times, groups, queue))

  def getTotal(seats: Int, times: Int, groups: Int, queue: Seq[Int]) = {
    def getData(data: (Int, Long, Boolean), currTimes: Int, acc: Long) =
      if (data._3) (data)
      else (currTimes - data._1, acc - data._2, true)

    def getTaken(currentQueue: Seq[Int]) = {
      def getTaken(cq: Seq[Int], size: Int, acc: Long): (Seq[Int], Long) =
        if (size >= groups) (cq, acc)
        else if (acc + cq.head > seats) (cq, acc)
        else (getTaken(cq.tail :+ cq.head, size + 1, acc + cq.head))

      getTaken(currentQueue, 0, 0)
    }

    def getTotal(currTimes: Int, currQueue: Seq[Int], cycles: Map[String, (Int, Long, Boolean)], acc: Long): Long = {
      if (currTimes == times) (acc)
      else {
        val queueText = currQueue.toString
        if (cycles.contains(queueText)) {
          val data = getData(cycles(queueText), currTimes, acc)
          if (currTimes + data._1 < times) {
            val count: Int = ((times - currTimes) / data._1)
            getTotal(currTimes + (data._1 * count), currQueue, cycles + (queueText -> data), acc + (data._2 * count))
          } else {
            val taken = getTaken(currQueue)
            getTotal(currTimes + 1, taken._1, cycles, acc + taken._2)
          }
        } else {
          val taken = getTaken(currQueue)
          getTotal(currTimes + 1, taken._1, cycles + (queueText -> (currTimes, acc, false)), acc + taken._2)
        }
      }
    }

    getTotal(0, queue, Map[String, (Int, Long, Boolean)](), 0)
  }
}