package A1

import A1.A1_1_PatternsToTrie._
import org.scalatest.FunSuite

class A1_1_PatternsToTrieTest extends FunSuite {

  test("empty trie") {
    val trie = Trie()
    assert(!trie.has("A"))
  }

  test("ATA") {
    val trie = Trie("ATA")

    assert(trie.has("ATA"))

    assert(!trie.has("AT"))
    assert(!trie.has("TA"))
    assert(!trie.has("A"))

    val edges = nodeToEdges(trie.root)
    val patterns = edgesToPatterns(edges)
    assert(patterns === Set("ATA"))
  }

  test("AT AG AC") {
    val trie = Trie("AT", "AG", "AC")

    assert(trie.has("AT"))
    assert(trie.has("AG"))
    assert(trie.has("AC"))
    assert(!trie.has("A"))
    assert(!trie.has("T"))
    assert(!trie.has("G"))
    assert(!trie.has("C"))

    val edges = nodeToEdges(trie.root)
    val patterns = edgesToPatterns(edges)
    assert(patterns === Set("AT", "AG", "AC"))
  }

  test("ATAGA ATC GAT") {
    val trie = Trie("ATAGA", "ATC", "GAT")

    assert(trie.has("ATAGA"))
    assert(trie.has("ATC"))
    assert(trie.has("GAT"))

    val edges = nodeToEdges(trie.root)
    val patterns = edgesToPatterns(edges)
    assert(patterns === Set("ATAGA", "ATC", "GAT"))
  }

}
