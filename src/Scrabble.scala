import java.util.Scanner

object Scrabble extends App {
  val in = new Scanner(System.in)
  val dictionary = Range(0, in.nextInt).map(Word(in.next, _)).sorted
  val letters = in.next

  println(dictionary.find(_.canBeWrittenWith(letters)).get)
}

object Word {
  def apply(word: String, originalIndex: Int) = new Word(word, originalIndex)
}

class Word(val word: String, val originalIndex: Int) extends Ordered[Word] {
  def frequency(text: String) = text.groupBy(_.toChar).mapValues(_.size)

  def points: Int = {
    def getPoints(index: Int, acc: Int): Int = {
      if (index == word.length) (acc)
      else (word(index)) match {
        case 'd' | 'g' => getPoints(index + 1, acc + 2)
        case 'b' | 'c' | 'm' | 'p' => getPoints(index + 1, acc + 3)
        case 'f' | 'h' | 'v' | 'w' | 'y' => getPoints(index + 1, acc + 4)
        case 'k' => getPoints(index + 1, acc + 5)
        case 'j' | 'x' => getPoints(index + 1, acc + 8)
        case 'q' | 'z' => getPoints(index + 1, acc + 10)
        case default => getPoints(index + 1, acc + 1)
      }
    }

    getPoints(0, 0)
  }

  override def toString = word

  def compare(that: Word) =
    if (this.points == that.points) (this.originalIndex - that.originalIndex)
    else (that.points - this.points)

  def canBeWrittenWith(letters: String): Boolean = {
    val letterFrequency = frequency(letters)
    frequency(word).forall(p => letterFrequency.contains(p._1) && letterFrequency(p._1) >= p._2)
  }
}