package A3

import javax.annotation.Nullable

import common.BaseCodec

private object A3_4_SuffixTree {

  object SuffixTree {

    def apply(text: String): SuffixTree = {
      val sortedSuffixes = A3_2_SuffixArray.sortStartIndices(text)
      val lcps = calcLCPs(sortedSuffixes, text)

      val tree = new SuffixTree(text)
      var edge = tree.rootNode.addEdge(sortedSuffixes(0))
      for (lcpI <- lcps.indices) {
        val lcp = lcps(lcpI)
        val node = edge.getNodeAboveAtDepth(lcp)
        val nextSuffix = sortedSuffixes(lcpI + 1)
        edge = node.addEdge(nextSuffix+node.depth)
      }

      tree
    }

  }

  class SuffixTree private(private val text: String) {

    private val rootNode = new Node(depth = 0, parentEdge = null)

    def has(suffix: String): Boolean = {
      // TODO: is recursion a problem?
      rootNode.has(suffix)
    }

    def getEdges: Seq[(Int, Int)] = {
      rootNode.getEdges
    }

    private class Node(val depth: Int, @Nullable val parentEdge: Edge) {

      private[SuffixTree] val byteToEdge = new Array[Edge](5)

      private[SuffixTree] def addEdge(startChI: Int): Edge = {
        addEdge(startChI, text.length - 1, nextNode = null)
      }

      private[SuffixTree] def addEdge(startChI: Int, endChI: Int, @Nullable nextNode: Node): Edge = {
        val startCh = text.charAt(startChI)
        val startByte = BaseCodec.toByte(startCh)
        if (byteToEdge(startByte) != null) {
          throw new IllegalStateException(s"attempt to add already existing edge starting with $startCh")
        }
        val newEdge = new Edge(this, startChI, endChI, nextNode)
        byteToEdge(startByte) = newEdge
        newEdge
      }

      private[SuffixTree] def has(suffix: String): Boolean = {
        val ch = suffix(0)
        val byte = BaseCodec.toByte(ch)
        val edge = byteToEdge(byte)
        if (edge == null) {
          false
        } else {
          edge.has(suffix)
        }
      }

      private[SuffixTree] def getEdges: Seq[(Int, Int)] = {
        // TODO: how Array is converted to filter? what is the with flatMap?
        byteToEdge.toSeq
          .filter(_ != null)
          .flatMap(edge => {
            val nextEdges = if (edge.nextNode != null) edge.nextNode.getEdges else Seq[(Int, Int)]()
            (edge.startChI, edge.endChI+1) +: nextEdges
          })
      }

    }

    private class Edge(val parentNode: Node,
                       val startChI: Int,
                       var endChI: Int,
                       @Nullable var nextNode: Node) {

      private[SuffixTree] def getNodeAboveAtDepth(targetDepth: Int): Node = {
        if (parentNode.depth == targetDepth) {
          parentNode

        } else if (parentNode.depth > targetDepth) {
          // TODO: try without recursion
          parentNode.parentEdge.getNodeAboveAtDepth(targetDepth)

        } else {
          val newEndChI = startChI + (targetDepth - parentNode.depth) - 1
          val newNextNode = new Node(targetDepth, parentEdge = this)
          newNextNode.addEdge(newEndChI+1, endChI, nextNode)
          endChI = newEndChI
          nextNode = newNextNode
          newNextNode
        }
      }

      private[SuffixTree] def has(suffix: String): Boolean = {
        val edgeLen = endChI - startChI + 1
        if (suffix.length < edgeLen) {
          false
        } else {
          for (chI <- startChI to endChI) {
            if (suffix.charAt(chI-startChI) != text.charAt(chI)) {
              return false
            }
          }
          if (suffix.length == edgeLen) {
            nextNode == null
          } else {
            if (nextNode == null) {
              false
            } else {
              nextNode.has(suffix.substring(edgeLen))
            }
          }
        }
      }
    }

  }

  // TODO: can we incapsulate sortedSuffixes and text?
  def calcLCPs(sortedSuffixes: Array[Int], text: String): Array[Int] = {
    if (sortedSuffixes.length == 1) {
      return Array.emptyIntArray
    }

    val sufToI = new Array[Int](sortedSuffixes.length)
    for (i <- sortedSuffixes.indices) {
      sufToI(sortedSuffixes(i)) = i
    }

    val lcps = new Array[Int](sortedSuffixes.length - 1)
    lcps(0) = 0 // $ has lcp=0 with any other suffix
    var lastLCP = lcps(0)
    var sufI = sufToI(0)
    for (i <- lcps.indices) { // one iteration will be skipped because we do not need to compute lcp for last suffix
      val suf = sortedSuffixes(sufI)
      if (sufI == sortedSuffixes.length - 1) {
        lastLCP = 0
      } else {
        val calcLCPOffset = math.max(0, lastLCP - 1)
        lcps(sufI) = calcLCPOffset + calcLCP(suf + calcLCPOffset, sortedSuffixes(sufI + 1) + calcLCPOffset, text)
        lastLCP = lcps(sufI)
      }
      sufI = sufToI(suf + 1)
    }
    lcps
  }

  private def calcLCP(suf1: Int, suf2: Int, text: String): Int = {
    var lcp = 0
    while (text.charAt(suf1 + lcp) == text.charAt(suf2 + lcp)) {
      lcp += 1
    }
    lcp
  }

}
