import java.util.Scanner

object Node {
  def apply(index: Int, data: String) = {
    val in = new Scanner(data).useDelimiter(",")
    val (name, fullName, _, lat, long) = (in.next.split(":")(1), in.next, in.next, in.nextDouble.toRadians, in.nextDouble.toRadians)
    new Node(index, name, fullName.replace("\"", ""), lat, long, Map[String, Double]())
  }
}

class Node(
    val index: Int,
    val name: String,
    val fullName: String,
    val lat: Double,
    val long: Double,
    val links: Map[String, Double]) {

  def getLink(target: Node): Double = this.getLink(target.name)
  def getLink(target: String): Double = this.links.getOrElse(target, Double.PositiveInfinity)
  def addLink(target: Node) = new Node(index, name, fullName, lat, long, links + (target.name -> this.distanceTo(target)))

  override def toString = this.fullName
  override def hashCode() = this.index
  override def equals(other: Any) = other match {
    case that: Node => that.index == this.index
    case _ => false
  }

  def distanceTo(that: Node) = {
    val x = (this.long - that.long) * Math.cos((this.lat + that.lat) / 2)
    val y = this.lat - that.lat
    Math.sqrt(x * x + y * y) * 6371
  }
}

object Train extends App {
  def stationName(in: Scanner) = in.next.split(":")(1)
  def getGraph(in: Scanner, nodes: Seq[Node]) = {
    def getGraph(curr: Int, total: Int, acc: Map[String, Node]): Map[String, Node] = {
      if (curr == total) acc
      else {
        val source = acc(stationName(in))
        val target = acc(stationName(in))
        getGraph(curr + 1, total, acc + (source.name -> source.addLink(target)))
      }
    }

    getGraph(0, in.nextInt, nodes.map(n => n.name -> n).toMap)
  }

  def getShortestPath(startPoint: String, endPoint: String, graphByName: Map[String, Node]) = {
    val graphById = graphByName.map { case (key, value) => value.index -> value }
    val (u, v) = (graphByName(startPoint).index, graphByName(endPoint).index)

    def getShortestPath: Array[Array[(Double, Int)]] = {
      def getInitialValues = {
        Array.tabulate(graphById.size, graphById.size)((i, j) => {
          if (i == j) (0d, 0)
          else {
            val (from, to) = (graphById(i), graphById(j))
            val distance = from.getLink(to)
            (distance, if (distance == Double.PositiveInfinity) -1 else to.index)
          }
        })
      }

      def getShortestPath(k: Int, i: Int, j: Int, acc: Array[Array[(Double, Int)]]): Array[Array[(Double, Int)]] = {
        if (k == graphByName.size) acc
        else if (i == graphByName.size) getShortestPath(k + 1, 0, 0, acc)
        else if (acc(i)(k)._1.isInfinity || (j == graphByName.size)) getShortestPath(k, i + 1, 0, acc)
        else {
          if (acc(i)(k)._1 + acc(k)(j)._1 < acc(i)(j)._1) acc(i)(j) = (acc(i)(k)._1 + acc(k)(j)._1, acc(i)(k)._2)
          getShortestPath(k, i, j + 1, acc)
        }
      }

      getShortestPath(0, 0, 0, getInitialValues)
    }

    def buildPath = {
      val data = getShortestPath
      def buildPath(u: Int, acc: List[String]): List[String] =
        if (u == v) acc :+ graphById(u).fullName
        else buildPath(data(u)(v)._2, acc :+ graphById(u).fullName)

      if (data(u)(v)._2 == -1) List[String]("IMPOSSIBLE")
      else buildPath(data(u)(v)._2, List[String](graphById(u).fullName))
    }

    buildPath
  }

  val in = new Scanner(System.in)
  val (startPoint, endPoint, nodeCount, _) = (stationName(in), stationName(in), in.nextInt, in.nextLine)
  val graph = getGraph(in, Range(0, nodeCount).map(Node(_, in.nextLine)))
  println(getShortestPath(startPoint, endPoint, graph).mkString("\n"))
}