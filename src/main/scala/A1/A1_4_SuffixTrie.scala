package A1

import scala.collection.mutable

private object A1_4_SuffixTrie {

  def suffixTrieToEdges(suffixTrie: SuffixTrie): Seq[String] = {
    val queueOfNodes = mutable.Queue(suffixTrie.rootNode)
    var edges = Seq[String]()
    while (queueOfNodes.nonEmpty) {
      val node = queueOfNodes.dequeue()
      val nodeEdges = node.getEdges
      edges = edges ++ nodeEdges.map(_.text)
      for (nodeEdge <- nodeEdges) {
        if (nodeEdge.nextNode != null) {
          queueOfNodes.enqueue(nodeEdge.nextNode)
        }
      }
    }
    edges
  }

}
