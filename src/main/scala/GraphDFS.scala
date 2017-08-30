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
  private val vertices = new ListBuffer[Int]

  /**
    * Clear graph (edges and verticies)
    */
  def clear = {
    edgesList.clear
    stack.clear
    vertices.clear
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
          case Edge(vertexFrom, vertexTo) if ((vertexFrom == vertex  && !vertices.contains(row.vertexTo)) || (vertexTo == vertex && !vertices.contains(row.vertexTo))) => edges += row
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
    val edges:Option[List[Edge]] = Some(getEdgesByVertex(vertex))
    val result = edges match {
      case Some(edge) if (!edge.isEmpty) => edge
      case None | _ => {
        val v = stack.pop
        getEdgesByVertex(v)
      }
    }
    result
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
    * Get last vertex from vertices list or first vertex if vertices list null
    * @return
    */
  def getVertex():Int = {
    val result: Option[Int] = vertices.lastOption
    result match {
      case Some(x) => x
      case None => getFirstVertex()
    }
  }

  /**
    * Deepth First Traversal by vertex
    *
    * @return
    */
  def DFS(): List[Int] = {
    stack.push(getVertex())
    whileLoop(stack.size > 0) {
      val edgeOption = getEdges(getVertex()).headOption
      vertices += getVertex()
      edgeOption match {
        case Some(edge) => {
          if (edge.vertexTo != getVertex() && !stack.contains(edge.vertexTo)) {
            vertices += edge.vertexTo
          }
          stack.push(edge.vertexTo)
        }
        case None => None
      }
    }
    vertices.toList.distinct
  }

  /**
    * While loop function to avoid while loop in code
    * @param cond
    * @param block
    */
  def whileLoop(cond : =>Boolean)(block : =>Unit) : Unit =
    if(cond) {
      block
      whileLoop(cond)(block)
    }
}
