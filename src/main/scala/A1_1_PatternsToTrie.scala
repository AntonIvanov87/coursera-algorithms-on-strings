import scala.collection.mutable

object A1_1_PatternsToTrie {

  case class IndexedEdge(from: Int, base: Char, to: Int)

  def nodeToEdges(rootNode: Trie.Node): Set[IndexedEdge] = {
    var indexedEdges = Set[IndexedEdge]()
    case class IndexedNode(node: Trie.Node, index: Int)
    val nodesToProcess = mutable.Queue[IndexedNode](IndexedNode(rootNode, 0))
    var nextNodeIndex = 1
    while (nodesToProcess.nonEmpty) {
      val fromNode = nodesToProcess.dequeue()
      for (edge <- fromNode.node.outEdges()) {
        indexedEdges += IndexedEdge(fromNode.index, edge.char, nextNodeIndex)
        nodesToProcess.enqueue(IndexedNode(edge.toNode, nextNodeIndex))
        nextNodeIndex += 1
      }
    }
    indexedEdges
  }

  def edgesToPatterns(edges: Set[IndexedEdge]): Set[String] = {
    val fromIndexToEdges = new mutable.HashMap[Int, mutable.Set[IndexedEdge]] with mutable.MultiMap[Int, IndexedEdge]
    for (edge <- edges) {
      fromIndexToEdges.addBinding(edge.from, edge)
    }

    var completePatterns = Set[String]()
    val prefixesAndNextIndexes = mutable.Queue[(String, Int)](("", 0))
    while (prefixesAndNextIndexes.nonEmpty) {
      val prefixAndNextIndex = prefixesAndNextIndexes.dequeue()
      val nextEdges = fromIndexToEdges.remove(prefixAndNextIndex._2)
      if (nextEdges.isEmpty) {
        completePatterns += prefixAndNextIndex._1
      } else {
        for (nextEdge <- nextEdges.get) {
          prefixesAndNextIndexes.enqueue((prefixAndNextIndex._1 + nextEdge.base, nextEdge.to))
        }
      }
    }
    completePatterns
  }

}
