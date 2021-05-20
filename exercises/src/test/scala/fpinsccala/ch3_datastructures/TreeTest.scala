package fpinsccala.ch3_datastructures

import fpinscala.ch3_datastructures.{Branch, Leaf, Tree}
import org.scalatest.FunSuite

class TreeTest extends FunSuite {

  test("EX25 size") {
    val leaf = Leaf(1)

    assert(Tree.size(leaf) == 1)

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

  test("EX26 maximum") {
    assert(Tree.maximum(Branch(Leaf(2), Leaf(20))) == 20)
    assert(Tree.maximum(Branch(Leaf(20), Leaf(2))) == 20)

    assert(Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) == 4)
    assert(Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(3)))) == 4)
    assert(Tree.maximum(Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(3), Leaf(2)))) == 4)
    assert(Tree.maximum(Branch(Branch(Leaf(2), Leaf(4)), Branch(Leaf(3), Leaf(1)))) == 4)
  }

  test("EX27 depth") {
    val leaf = Leaf(1)
    assert(Tree.depth(leaf) == 1)
    assert(Tree.depth(Branch(leaf, leaf)) == 2)

    assert(Tree.depth(
      Branch(
        Branch(
          leaf, leaf), leaf)) == 3)
  }

  test("EX28 map"){
assert(
    Tree.map(
    Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))))(_ * 2)
  ==

    Branch(Branch(Leaf(2), Leaf(4)), Branch(Leaf(6), Leaf(8)))
    )
  }


}
