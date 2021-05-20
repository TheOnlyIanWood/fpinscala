package fpinsccala.ch3_datastructures

import fpinscala.ch3_datastructures.{Branch, Leaf, Tree}
import org.scalatest.FunSuite

class TreeTest extends FunSuite {

  test("EX25 size 1") {
    val leaf = Leaf(1)
    assert(Tree.size(leaf) == 1)


  }

  test("EX25 size 2") {
    val leaf = Leaf(1)


    assert(Tree.size(Branch(leaf, leaf)) == 3)

    assert(Tree.size(
      Branch(Branch(leaf, leaf),
        Branch(leaf, leaf))
    ) == 7)


    assert(Tree.size(
      Branch(Branch(Branch(leaf, leaf), Branch(leaf, leaf)),
        Branch(leaf, leaf))
    ) == 11)

  }

}
