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

  /**
    * Clear graph (edges and verticies)
    */
  def clear = {
    edgesList.clear()
    stack.clear()
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
          case Edge(vertexFrom, vertexTo) if ((vertexFrom == vertex && !stack.contains(vertexFrom)) || (vertexTo == vertex && !stack.contains(vertexFrom))) => edges += row
          case _ => Nil
        }
    )
    edges.toList.filter(_ != Nil)
  }

  /**
    * Breadth First Traversal by vertex
    *
    * @param vertex
    * @return
    */
  def DFS(vertex: Int): List[Int]= {
    val verticies = new ListBuffer[Int]
    edgesList.foreach(
      row =>
        row match {
          case Edge(vertexFrom , _ ) if (vertexFrom == vertex && !stack.contains(vertexFrom)) => {
            verticies += vertexFrom
            stack.push(vertexFrom)
            DFS(vertexFrom)
          }
          case Edge( _ , vertexTo) if (vertexTo == vertex && !stack.contains(vertexTo)) => {
            verticies += vertexTo
            stack.push(vertexTo)
            DFS(vertexTo)
          }
          case _ => Nil
        }
    )
    verticies.toList.filter(_ != Nil)
  }

}
