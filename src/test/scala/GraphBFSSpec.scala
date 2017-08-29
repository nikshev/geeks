import org.scalatest._

class GraphBFSSpec extends FlatSpec with Matchers {
  "GraphBFS clear " should " be successfully" in {
    GraphBFS.clear
    val result = GraphBFS.BFS(2)
    result should equal (List(2))
  }

  "GraphBFS add edges" should " be successfully" in {
    GraphBFS.clear
    GraphBFS.addEdge(0, 1).toList should equal(List(Edge(0, 1)))
    GraphBFS.addEdge(0, 2).toList should equal(List(Edge(0,1), Edge(0,2)))
    GraphBFS.addEdge(1, 2).toList should equal(List(Edge(0,1), Edge(0,2), Edge(1,2)))
    GraphBFS.addEdge(2, 0).toList should equal(List(Edge(0,1), Edge(0,2), Edge(1,2), Edge(2,0)))
    GraphBFS.addEdge(2, 3).toList should equal(List(Edge(0,1), Edge(0,2), Edge(1,2), Edge(2,0), Edge(2,3)))
    GraphBFS.addEdge(3, 3).toList should equal(List(Edge(0,1), Edge(0,2), Edge(1,2), Edge(2,0), Edge(2,3), Edge(3,3)))
  }

  "GraphBFS get edges " should " be successfully" in {
    GraphBFS.clear
    GraphBFS.addEdge(0, 1)
    GraphBFS.addEdge(0, 2)
    GraphBFS.addEdge(1, 2)
    GraphBFS.addEdge(2, 0)
    GraphBFS.addEdge(2, 3)
    GraphBFS.addEdge(3, 3)
    GraphBFS.getEdgesByVertex(2) should equal(List(Edge(0,2), Edge(1,2), Edge(2,0), Edge(2,3)))
  }

  "GraphBFS Breadth First Traversal for 2" should " be successfully" in {
    GraphBFS.clear
    GraphBFS.addEdge(0, 1)
    GraphBFS.addEdge(0, 2)
    GraphBFS.addEdge(1, 2)
    GraphBFS.addEdge(2, 0)
    GraphBFS.addEdge(2, 3)
    GraphBFS.addEdge(3, 3)
    val result = GraphBFS.BFS(2)
    result.contains(2) should be === true
    result.contains(0) should be === true
    result.contains(1) should be === true
    result.contains(3) should be === true
  }

  "GraphBFS Breadth First Traversal for 1" should " be successfully" in {
    GraphBFS.clear
    GraphBFS.addEdge(0, 1)
    GraphBFS.addEdge(0, 2)
    GraphBFS.addEdge(1, 2)
    GraphBFS.addEdge(2, 0)
    GraphBFS.addEdge(2, 3)
    GraphBFS.addEdge(3, 3)
    val result = GraphBFS.BFS(1)
    result.contains(2) should be === true
    result.contains(0) should be === true
    result.contains(1) should be === true
  }

  "GraphBFS Breadth First Traversal for 3" should " be successfully" in {
    GraphBFS.clear
    GraphBFS.addEdge(0, 1)
    GraphBFS.addEdge(0, 2)
    GraphBFS.addEdge(1, 2)
    GraphBFS.addEdge(2, 0)
    GraphBFS.addEdge(2, 3)
    GraphBFS.addEdge(3, 3)
    val result = GraphBFS.BFS(3)
    result.contains(2) should be === true
    result.contains(0) should be === true
    result.contains(1) should be === true
    result.contains(3) should be === true
  }

}
