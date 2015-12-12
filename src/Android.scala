import java.util.Scanner

object Player extends App {

  val in = new Scanner(System.in)
  val (nbFloors, _, nbRounds, exitFloor, exitPos, _, _) = (in.nextInt, in.nextInt, in.nextInt, in.nextInt, in.nextInt, in.nextInt, in.nextInt)
  val elevators = Range(0, in.nextInt).map(n => (in.nextInt, in.nextInt))
  printSequence(in, nbFloors, nbRounds, exitFloor, exitPos, elevators)

  def printSequence(
    in: Scanner,
    nbFloors: Int,
    nbRounds: Int,
    exit: Int,
    exitPos: Int,
    elevators: Seq[(Int, Int)]): Unit = {

    def printSequence(rounds: Int, hasBlock: Seq[Boolean]): Unit = {
      if (rounds < nbRounds) {
        val (floor, pos, direction) = (in.nextInt, in.nextInt, in.next)
        if ((floor >= 0)
          && (!hasBlock(floor))
          && (needsBlock(floor, pos, direction))) {
          println("BLOCK")
          printSequence(rounds + 1, hasBlock.patch(floor, Seq(true), 1))
        } else {
          println("WAIT")
          printSequence(rounds + 1, hasBlock)
        }
      }
    }

    def needsBlock(floor: Int, pos: Int, direction: String) = {
      if (floor == exitFloor) {
        if ("LEFT".equals(direction)) (pos < exitPos) else (pos > exitPos)
      } else {
        val elevator = elevators.find(_._1 == floor).get
        if ("LEFT".equals(direction)) (elevator._2 > pos)
        else (elevator._2 < pos)
      }
    }

    printSequence(0, Range(0, nbFloors).map(n => false))
  }
}