import java.util.Scanner

object Conway extends App {

  val in = new Scanner(System.in)
  println(getResult(in.nextInt, in.nextInt).mkString(" "))
  
  def getResult(start: Int, index: Int) = {
    def accumList(list: List[Int]) = {
        def accumList(curr: List[Int], acc: List[Int]): List[Int] = {
            if (curr.isEmpty) acc
            else accumList(
                  curr.dropWhile(_ == curr.head), 
                  acc :+ curr.takeWhile(_ == curr.head).size :+ curr.head)
        }
        
        accumList(list, List[Int]())
    }

    def getResult(currIndex: Int, list: List[Int]): List[Int] = {
      if (currIndex == index) list
      else getResult(currIndex + 1, accumList(list)) 
    }
    
    getResult(1, List[Int](start))
  }
}