import java.util.Scanner
import scala.annotation.tailrec
import java.io.FileInputStream
import java.io.File

object Surface extends App {

  val in = new Scanner(new FileInputStream(new File("/tmp/test.txt")))
  val (w, h) = (in.nextInt, in.nextInt)
  val map = Range(0, h).map(n => in.next.toCharArray)
  Range(0, in.nextInt).foreach(n => println(getFloodFill(in.nextInt, in.nextInt, map)))

  def getFloodFill(x: Int, y: Int, map: Seq[Array[Char]]): Long = {
    @tailrec def getFloodFill(visited: List[(Int, Int)], queue: List[(Int, Int)], acc: Long): Long = {
      if (queue.isEmpty) (acc)
      else {
        val c = queue.head
        if (visited.contains(c)) (getFloodFill(visited, queue.tail, acc))
        else (getFloodFill(
          visited :+ c,
          queue.tail ++ List((c._1 - 1, c._2), (c._1 + 1, c._2), (c._1, c._2 - 1), (c._1, c._2 + 1))
            .filter(
              nc =>
                map.isDefinedAt(nc._1)
                  && map(nc._1).isDefinedAt(nc._2)
                  && !visited.contains(nc)
                  && map(nc._1)(nc._2) == 'O'),
          acc + 1))
      }
    }

    if (map(y)(x) == '#') (0)
    else (getFloodFill(
      List[(Int, Int)](),
      List[(Int, Int)]((y, x)),
      0))
  }
}