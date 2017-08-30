import GraphDFS.Edge
import org.scalatest._

class GraphDFSSpec extends FlatSpec with Matchers {

  "GraphDFS add edges" should " be successfully" in {
    GraphDFS.clear
    GraphDFS.addEdge(0, 1).toList should equal(List(Edge(0, 1)))
    GraphDFS.addEdge(1, 4).toList should equal(List(Edge(0,1), Edge(1,4)))
    GraphDFS.addEdge(4, 2).toList should equal(List(Edge(0,1), Edge(1,4), Edge(4,2)))
    GraphDFS.addEdge(4, 3).toList should equal(List(Edge(0,1), Edge(1,4), Edge(4,2), Edge(4,3)))
    GraphDFS.addEdge(2, 0).toList should equal(List(Edge(0,1), Edge(1,4), Edge(4,2), Edge(4,3), Edge(2,0)))
    GraphDFS.addEdge(3, 0).toList should equal(List(Edge(0,1), Edge(1,4), Edge(4,2), Edge(4,3), Edge(2,0), Edge(3,0)))
  }

  "GraphDFS getEdgesByVertex" should " be successfully" in {
    GraphDFS.clear
    GraphDFS.addEdge(1, 2)
    GraphDFS.addEdge(1, 3)
    GraphDFS.addEdge(2, 4)
    GraphDFS.addEdge(2, 6)
    GraphDFS.addEdge(3, 7)
    GraphDFS.addEdge(6, 5)
    GraphDFS.getEdgesByVertex(1) should contain theSameElementsAs List(Edge(1,2), Edge(1,3))
  }

  "GraphDFS getEdges" should " be successfully" in {
    GraphDFS.clear
    GraphDFS.addEdge(1, 2)
    GraphDFS.addEdge(1, 3)
    GraphDFS.addEdge(2, 4)
    GraphDFS.addEdge(2, 6)
    GraphDFS.addEdge(3, 7)
    GraphDFS.addEdge(6, 5)
    GraphDFS.getEdges(1) should contain theSameElementsAs List(Edge(1,2), Edge(1,3))
  }

  "GraphDFS getFirstVertex" should " be successfully" in {
    GraphDFS.clear
    GraphDFS.addEdge(1, 2)
    GraphDFS.addEdge(1, 3)
    GraphDFS.addEdge(2, 4)
    GraphDFS.addEdge(2, 6)
    GraphDFS.addEdge(3, 7)
    GraphDFS.addEdge(6, 5)
    GraphDFS.getFirstVertex() should be === 1
  }

  "GraphDFS Depth First Traversal or DFS for a Graph  case 1" should " be successfully" in {
    GraphDFS.clear
    GraphDFS.addEdge(1, 2)
    GraphDFS.addEdge(1, 3)
    GraphDFS.addEdge(2, 4)
    GraphDFS.addEdge(2, 6)
    GraphDFS.addEdge(3, 7)
    GraphDFS.addEdge(6, 5)
    GraphDFS.DFS() should contain theSameElementsAs List(1, 2, 4, 6, 5, 3, 7)
  }

  "GraphDFS Depth First Traversal or DFS for a Graph  case 2" should " be successfully" in {
    GraphDFS.clear
    GraphDFS.addEdge(0, 1)
    GraphDFS.addEdge(1, 4)
    GraphDFS.addEdge(4, 2)
    GraphDFS.addEdge(4, 3)
    GraphDFS.addEdge(2, 0)
    GraphDFS.addEdge(3, 0)
    GraphDFS.DFS() should contain theSameElementsAs List(0, 1, 4, 2, 3)
  }

}
