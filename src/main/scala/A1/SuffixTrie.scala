package A1

import common.BaseCodec

import scala.collection.mutable

class SuffixTrie(text: String) {

  val rootNode = new Node
  private val textBytes = BaseCodec.toBytes(text)
  for (startIndex <- textBytes.indices) {
    rootNode.add(Edge(startIndex, textBytes.length - 1, null))
  }

  def has(thatText: String): Boolean = {
    if (thatText.isEmpty) {
      return false
    }

    val thatBytes = BaseCodec.toBytes(thatText)
    var curNode = rootNode
    var thatBytesI = 0
    while (true) {
      val edge = curNode.getEdge(thatBytes(thatBytesI))
      if (edge == null) {
        return false
      }

      for (i <- edge.fromIndex to edge.toIndex) {
        if (thatBytes(thatBytesI) != textBytes(i)) {
          return false
        }
        thatBytesI += 1
        if (thatBytesI == thatBytes.length) {
          return true
        }
      }

      if (edge.nextNode == null) {
        return false
      }
      curNode = edge.nextNode
    }
    false
  }

  def substrings: Iterator[String] = {
    case class PrefixEdgeOffset(prefix: String, edge: Edge, offset: Int)
    val queue = mutable.Queue[PrefixEdgeOffset]()
    rootNode.getEdges.foreach(edge => queue.enqueue(PrefixEdgeOffset("", edge, 0)))
    new Iterator[String] {
      override def hasNext: Boolean = queue.nonEmpty

      override def next: String = {
        val PrefixEdgeOffset(prefix, edge, offset) = queue.dequeue

        val result = prefix + edge.baseAt(offset)

        if (edge.fromIndex + offset == edge.toIndex) {
          if (edge.nextNode != null) {
            edge.nextNode.getEdges.foreach(e => queue.enqueue(PrefixEdgeOffset(result, e, 0)))
          }
        } else {
          queue.enqueue(PrefixEdgeOffset(result, edge, offset+1))
        }

        result
      }
    }
  }

  private[SuffixTrie] class Node {

    private val edges: Array[Edge] = new Array[Edge](5)

    def getEdges: Set[Edge] = {
      edges.filter(_ != null).toSet
    }

    private[SuffixTrie] def add(edge: Edge): Unit = {
      val fromByte = textBytes(edge.fromIndex)
      val curEdge = edges(fromByte)
      edges(fromByte) =
        if (curEdge == null) {
          edge
        } else {
          curEdge.add(edge)
        }
    }

    private[SuffixTrie] def getEdge(base: Byte): Edge = {
      edges(base)
    }
  }

  private[SuffixTrie] case class Edge(fromIndex: Int, toIndex: Int, nextNode: Node) {

    def text: String = {
      val numOfElems = toIndex-fromIndex+1
      val chars = new Array[Char](numOfElems)
      for (i <- 0 until numOfElems) {
        chars(i) = BaseCodec.toBase(textBytes(fromIndex+i))
      }
      String.valueOf(chars)
    }

    def baseAt(i: Int): Char = {
      BaseCodec.toBase(textBytes(fromIndex + i))
    }

    private[SuffixTrie] def add(thatEdge: Edge): Edge = {
      var offset = 0
      while (textBytes(fromIndex + offset) == textBytes(thatEdge.fromIndex + offset)) {
        offset += 1
        if (fromIndex + offset > toIndex) {
          nextNode.add(Edge(thatEdge.fromIndex + offset, thatEdge.toIndex, thatEdge.nextNode))
          return this
        }
      }
      val newNode = new Node
      newNode.add(Edge(thatEdge.fromIndex + offset, thatEdge.toIndex, thatEdge.nextNode))
      newNode.add(Edge(fromIndex + offset, toIndex, nextNode))
      Edge(fromIndex, fromIndex + offset - 1, newNode)
    }
  }

}
