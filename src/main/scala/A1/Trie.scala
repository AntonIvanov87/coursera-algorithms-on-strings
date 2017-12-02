package A1


class Trie(patterns: Seq[String]) {
  import Trie._

  private val _root: MidNode = new MidNode

  add(patterns)

  def root: Node = _root

  def has(pattern: String): Boolean = {
    if (pattern.isEmpty) {
      return false
    }

    val bytes = BaseCodec.toBytes(pattern)
    var curNode: Node = _root
    for (i <- bytes.indices) {
      val base = bytes(i)
      val nextNode = curNode.nextNode(base)
      if (nextNode == null) {
        return false
      }
      curNode = nextNode
    }

    curNode.isEndOfPattern
  }

  private def add(patterns: TraversableOnce[String]): Unit = {
    for (pattern <- patterns) {
      add(pattern)
    }
  }

  private def add(pattern: String): Unit = {
    val bytes = BaseCodec.toBytes(pattern)

    var currentNode = _root
    for (i <- 0 until bytes.length - 1) {
      val base = bytes(i)
      val nextNode = currentNode.nextNode(base)
      val nextMidNode = nextNode match {
        case null =>
          val newMidNode = new MidNode
          currentNode.replaceNode(base, newMidNode)
          newMidNode
        case midNode: MidNode => midNode
        case _: LeafNode =>
          val newMidNode = new MidNode
          newMidNode.markAsEnd()
          currentNode.replaceNode(base, newMidNode)
          newMidNode
      }
      currentNode = nextMidNode
    }

    val base = bytes.last
    val nextNode = currentNode.nextNode(base)
    nextNode match {
      case null => currentNode.replaceNode(base, new LeafNode)
      case node: Node => node.markAsEnd()
    }
  }

}

object Trie {

  def apply(patterns: String*): Trie = new Trie(patterns)

  trait Node {

    def nextNode(base: Char): Option[Node] = {
      Option.apply(nextNode(BaseCodec.toByte(base)))
    }

    def isEndOfPattern: Boolean

    def outEdges(): Set[ToEdge]

    protected[Trie] def nextNode(b: Byte): Node

    protected[Trie] def markAsEnd(): Unit

  }

  private class MidNode extends Node {

    private val pointersToBases: Array[Node] = new Array[Node](4)
    private var endOfPattern: Boolean = false

    override def isEndOfPattern: Boolean = endOfPattern

    override def outEdges(): Set[ToEdge] = {
      pointersToBases.zipWithIndex
        .filter(nodeAndIndex => nodeAndIndex._1 != null)
        .map(nodeAndIndex => ToEdge(BaseCodec.toBase(nodeAndIndex._2.toByte), nodeAndIndex._1))
        .toSet
    }

    override protected[Trie] def nextNode(b: Byte): Node = pointersToBases(b)

    override protected[Trie] def markAsEnd(): Unit = {
      endOfPattern = true
    }

    private[Trie] def replaceNode(byte: Byte, node: Node): Unit = {
      pointersToBases(byte) = node
    }

  }

  private class LeafNode extends Node {
    override def isEndOfPattern: Boolean = true

    override def outEdges(): Set[ToEdge] = Set[ToEdge]()

    override protected[Trie] def nextNode(b: Byte): Node = null

    override protected[Trie] def markAsEnd(): Unit = {}
  }

  case class ToEdge(char: Char, toNode: Node)

  private object BaseCodec {

    def toBytes(bases: String): Array[Byte] = {
      val result = new Array[Byte](bases.length)
      for (i <- 0 until bases.length) {
        result(i) = toByte(bases(i))
      }
      result
    }

    def toByte(symbol: Char): Byte = {
      symbol match {
        case 'A' => 0
        case 'C' => 1
        case 'G' => 2
        case 'T' => 3
        case _ => throw new IllegalArgumentException(s"unknown symbol '$symbol' (${symbol.toInt})")
      }
    }

    def toBase(byte: Byte): Char = {
      byte match {
        case 0 => 'A'
        case 1 => 'C'
        case 2 => 'G'
        case 3 => 'T'
        case _ => throw new IllegalArgumentException(s"unknown byte $byte")
      }
    }

  }

}
