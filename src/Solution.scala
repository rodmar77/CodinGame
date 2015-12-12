import scala.util._
import scala.collection.mutable._
import scala.io.StdIn._
import scala.annotation.tailrec
import java.util.Scanner
import java.io.File

object Solution extends App {
  val chars = Map[String, Character](
    ".-" -> 'A', "-..." -> 'B', "-.-." -> 'C', "-.." -> 'D', "." -> 'E', "..-." -> 'F',
    "--." -> 'G', "...." -> 'H', ".." -> 'I', ".---" -> 'J', "-.-" -> 'K', ".-.." -> 'L',
    "--" -> 'M', "-." -> 'N', "---" -> 'O', ".--." -> 'P', "--.-" -> 'Q', ".-." -> 'R',
    "..." -> 'S', "-" -> 'T', "..-" -> 'U', "...-" -> 'V', ".--" -> 'W', "-..-" -> 'X',
    "-.--" -> 'Y', "--.." -> 'Z')

  val morse = chars.map { case (k, v) => v -> k.length }
  val dict = new Dictionary()

  val s = new Scanner(new File("/tmp/test.txt"))
  val text = s.nextLine
  (1 to s.nextInt).foreach { n => dict += s.next }
  println(getTotalFor(text))

  def getTotalFor(text: String): Long = {
    val cache = HashMap[Integer, Long]()

    def wordSize(word: String): Int = word.map(morse(_)).sum
    def getTotalFor(text: String, index: Int): Long = {
      if (index == text.length) return (cache.getOrElseUpdate(index, 1L))
      else cache.getOrElseUpdate(index, getWordCount(text, index)
        .map { case (k, v) => v * getTotalFor(text, index + k) }
        .foldLeft(0L)(_ + _))
    }

    def getWordCount(text: String, index: Int): Map[Int, Long] = {
      def getWordCount(currentWord: String, index: Int, count: Map[Int, Long]): Map[Int, Long] = {
        Range
          .inclusive(1, 4)
          .takeWhile(index + _ <= text.length)
          .map(i => (text.substring(index, index + i), i))
          .filter(chars contains _._1)
          .map(p => (currentWord + chars(p._1), p._2))
          .foreach(p => {
            if (dict.hasWord(p._1)) {
              val key = wordSize(p._1)
              if (count.contains(key)) count += (key -> (count(key) + 1L))
              else count += (key -> 1L)
            }

            if (dict.hasPrefix(p._1)) getWordCount(p._1, index + p._2, count)
          })

        count
      }

      getWordCount("", index, Map[Int, Long]())
    }

    getTotalFor(text, 0)
  }
}

object Atom {
  def apply(): Atom = new Atom(Map[Char, Atom]())
  def apply(value: String): Atom = new Atom(Map[Char, Atom]()) += value.tail
}

class Atom(val atoms: Map[Char, Atom]) {

  def hasSuffix(text: String): Boolean = {
    @tailrec def suffix(atom: Atom, value: String): Boolean = {
      if (value.isEmpty) (true)
      else if (!atom.atoms.contains(value.head)) (false)
      else if (value.length > 1) suffix(atom.atoms(value.head), value.tail)
      else true
    }

    suffix(this, text)
  }

  def +=(value: String): this.type = {
    val key = value.head
    if (atoms.contains(key)) {
      if (value.length > 1) atoms(key) += value.tail
    } else if (value.length == 1) atoms += (key -> Atom())
    else atoms += (key -> Atom(value))
    this
  }
}

class Dictionary(val dict: Map[Char, Atom], val words: Set[String]) {

  def this() = this(HashMap[Char, Atom](), HashSet[String]())

  def +=(word: String): this.type = {
    val key = word.head
    if (!dict.contains(key)) (dict += (key -> Atom(word)))
    else if (word.length > 1) dict(key) += word.tail

    words += word
    this
  }

  def hasWord(word: String) = words.contains(word)
  def hasPrefix(value: String): Boolean = {
    if (value.isEmpty) (true)
    else if (!dict.contains(value.head)) (false)
    else if (value.length() > 1) (dict(value.head).hasSuffix(value.tail))
    else (true)
  }
}