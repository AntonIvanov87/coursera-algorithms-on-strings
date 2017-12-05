package common

object BaseCodec {

  def toBytes(bases: String): Array[Byte] = {
    val result = new Array[Byte](bases.length)
    for (i <- 0 until bases.length) {
      result(i) = toByte(bases(i))
    }
    result
  }

  def toByte(symbol: Char): Byte = {
    symbol match {
      case '$' => 0
      case 'A' => 1
      case 'C' => 2
      case 'G' => 3
      case 'T' => 4
      case _ => throw new IllegalArgumentException(s"unknown symbol $symbol")
    }
  }

  def toBase(byte: Byte): Char = {
    byte match {
      case 0 => '$'
      case 1 => 'A'
      case 2 => 'C'
      case 3 => 'G'
      case 4 => 'T'
      case _ => throw new IllegalArgumentException(s"unknown byte $byte")
    }
  }

}
