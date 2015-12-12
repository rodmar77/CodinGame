import java.util.Scanner
import scala.annotation.tailrec

object SuperComputer extends App {
  val in = new Scanner(System.in)
  val calcs = Array.tabulate(in.nextInt)(
    n => (in.nextInt, in.nextInt))
    .map(n => (n._1, n._1 + n._2))
    .sortWith((a, b) => (a._2.compare(b._2) < 0))

  println(getCount(calcs))

  def getCount(calcs: Array[(Int, Int)]): Int = {
    @tailrec def getCount(index: Int, free: Array[Boolean], acc: Int): Int = {
      def newFree(seq: Seq[Int]) = Array.tabulate(calcs.last._2)(i => if (seq.contains(i)) (false) else (free(i)))

      if (index == calcs.size) (acc)
      else {
        val seq = Range(calcs(index)._1, calcs(index)._2)
        if (seq.forall(free(_))) (getCount(index + 1, newFree(seq), acc + 1))
        else (getCount(index + 1, free, acc))
      }
    }

    getCount(0, Array.fill(calcs.last._2)(true), 0)
  }
}