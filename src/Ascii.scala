import java.util.Scanner
import java.io.FileInputStream
import java.io.File

object Ascii extends App {

  val in = new Scanner(new FileInputStream(new File("/tmp/test.txt")));
  val (charWidth, charHeight, text) = (in.nextLine.toInt, in.nextLine.toInt, in.nextLine.toLowerCase)
  val charMap = getCharMap
  val convert = getText(text.map(charMap.getOrElse(_, charMap('?'))))
  println(convert)

  def getText(vector: Seq[List[String]]): String = {
    def getText(index: Int, acc: List[String]): List[String] = {
      if (index == charHeight) (acc)
      else (getText(index + 1, acc :+ Range(0, vector.size).map(vector(_)(index)).mkString))
    }

    getText(0, List[String]()).mkString("\n")
  }

  def getCharMap = {
    def getCharMap(chars: String, index: Int, alphabet: List[String], map: Map[Char, List[String]]): Map[Char, List[String]] = {
      val key = chars(index % chars.length)
      if (index == alphabet.size) (map)
      else getCharMap(chars, index + 1, alphabet, map + (key -> (map(key) :+ alphabet(index))))
    }

    val chars = "abcdefghijklmnopqrstuvwxyz?"
    getCharMap(
      chars,
      0,
      Range(0, charHeight).flatMap(n => in.nextLine.sliding(charWidth, charWidth)).toList,
      chars.map { (_, List[String]()) }.toMap)
  }
}