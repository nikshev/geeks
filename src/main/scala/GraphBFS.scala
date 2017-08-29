import scala.collection.mutable.ListBuffer

/**
  * http://www.geeksforgeeks.org/breadth-first-traversal-for-a-graph/
  * Breadth First Traversal (or Search) for a graph is similar to Breadth First Traversal of a tree.
  * The only catch here is, unlike trees, graphs may contain cycles, so we may come to the same node again.
  * To avoid processing a node more than once, we use a boolean visited array.
  * For simplicity, it is assumed that all vertices are reachable from the starting vertex.
  */
/**
  * Edge case class
  * @param vertexFrom
  * @param vertexTo
  */
case class Edge(vertexFrom: Int, vertexTo: Int)

/**
  * Vertex case class
  * @param vertex
  * @param visited
  */
case class Vertex(vertex: Int, visited: Boolean = false)

/**
  * Main object
  */
object GraphBFS {
  private val edgesList = new ListBuffer[Edge]
  private val vertexList = new ListBuffer[Vertex]

  /**
    * Clear graph (edges and verticies)
    */
  def clear = {
    edgesList.clear()
    vertexList.clear()
  }

  /**
    * Add edge to graph
    * @param vertexFrom
    * @param vertexTo
    * @return
    */
  def addEdge(vertexFrom: Int, vertexTo: Int) = {
    edgesList += Edge(vertexFrom, vertexTo)
  }

  /**
    * Cgeck if vertex visited
    * @param vertex
    * @return
    */
  def isVisited(vertex:Int):Boolean ={
    vertexList.contains(Vertex(vertex,true))
  }

  /**
    * Get edges by vertex
    * @param vertex
    * @return
    */
  def getEdgesByVertex(vertex: Int): List[Edge] = {
    val edges = new ListBuffer[Edge]
      edgesList.foreach(
        row =>
          row match {
            case Edge(vertexFrom, vertexTo) if ((vertexFrom == vertex && !isVisited(vertexFrom)) || (vertexTo == vertex&& !isVisited(vertexFrom))) => edges += row
            case _ => Nil
          }
      )
    edges.toList.filter(_ != Nil)

  }

  /**
    * Breadth First Traversal by vertex
    * @param vertex
    * @return
    */
  def BFS(vertex: Int): List[Int]= {
    val edges = getEdgesByVertex(vertex)
    vertexList += Vertex(vertex, true)
    val traverse = new ListBuffer[Int]
    traverse += vertex
    edges.foreach { edge =>
      edge match {
        case Edge(vertexFrom, vertexTo) => {
          if (!isVisited(vertexFrom) && vertexFrom != vertex) {
            vertexList += Vertex(vertexFrom, true)
            traverse ++= BFS(vertexFrom)
          }
          if (!isVisited(vertexTo)&& vertexTo != vertex) {
            vertexList += Vertex(vertexTo, true)
            traverse ++= BFS(vertexTo)
          }
        }
        case _ => Nil
      }
    }
    traverse.toList.filter(_ != Nil)
  }
}
