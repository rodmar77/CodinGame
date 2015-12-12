import java.util.Scanner

object MimeType extends App {
  val in = new Scanner(System.in)
  val (n, q) = (in.nextInt, in.nextInt)
  val types = Range(0, n).map(k => in.next.toLowerCase -> in.next).toMap

  in.nextLine
  Range(0, q).foreach { x =>
    val name = in.nextLine
    val index = name.lastIndexOf('.')
    if (index < 0) (println("UNKNOWN"))
    else (println(types.getOrElse(name.substring(index + 1).toLowerCase, "UNKNOWN")))
  }
}