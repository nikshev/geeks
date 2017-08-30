import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by yshkurnykov on 29.08.2017.
  * Depth First Traversal or DFS for a Graph
  * http://www.geeksforgeeks.org/depth-first-traversal-for-a-graph/
  * https://www.tutorialspoint.com/data_structures_algorithms/depth_first_traversal.htm
  */
object GraphDFS {
  /**
    * Edge case class
    *
    * @param vertexFrom
    * @param vertexTo
    */
  case class Edge(vertexFrom: Int, vertexTo: Int)

  /**
    * Vertex case class
    *
    * @param vertex
    * @param visited
    */
  case class Vertex(vertex: Int, visited: Boolean = false)

  private val edgesList = new ListBuffer[Edge]
  private val stack = new mutable.Stack[Int]
  private val verticies = new ListBuffer[Int]

  /**
    * Clear graph (edges and verticies)
    */
  def clear = {
    edgesList.clear
    stack.clear
    verticies.clear
  }

  /**
    * Add edge to graph
    *
    * @param vertexFrom
    * @param vertexTo
    * @return
    */
  def addEdge(vertexFrom: Int, vertexTo: Int) = {
    edgesList += Edge(vertexFrom, vertexTo)
  }

  /**
    * Get edges by vertex
    *
    * @param vertex
    * @return
    */
  def getEdgesByVertex(vertex: Int): List[Edge] = {
    val edges = new ListBuffer[Edge]
    edgesList.foreach(
      row =>
        row match {
          case Edge(vertexFrom, vertexTo) if ((vertexFrom == vertex  && !verticies.contains(row.vertexTo)) || (vertexTo == vertex && !verticies.contains(row.vertexTo))) => edges += row
          case _ => Nil
        }
    )
    edges.toList.filter(_ != Nil)
  }

  /**
    * Get edges in any case (if your stack has elements)
    * @param vertex
    * @return
    */
  def getEdges(vertex: Int): List[Edge] = {
    var edges = getEdgesByVertex(vertex)
    while (edges.isEmpty && stack.size != 0) {
      edges = getEdgesByVertex(stack.pop)
    }
    edges
  }

  /**
    * Get first vertex
    * @return
    */
  def getFirstVertex():Int = {
    val headOption = edgesList.headOption
    headOption match {
      case Some(edge) => edge.vertexFrom
      case None => -1
    }
  }
  /**
    * Deepth First Traversal by vertex
    *
    * @return
    */
  def DFS(): List[Int] = {
    var vertex = getFirstVertex()
    stack.push(vertex)
    while (stack.size > 0) {
      val edgeOption = getEdges(vertex).headOption
      edgeOption match {
        case Some(edge) => {
          verticies += vertex
          if (edge.vertexTo != vertex && !stack.contains(edge.vertexTo)) {
            vertex = edge.vertexTo
          }
          stack.push(vertex)
        }
        case None => ;
      }
    }
    verticies.toList.distinct
  }

}
