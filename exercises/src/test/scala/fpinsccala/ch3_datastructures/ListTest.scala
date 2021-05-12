package fpinsccala.ch3_datastructures

import org.scalatest.FunSuite

import fpinscala.ch3_datastructures.{List => fpList}
import fpinscala.ch3_datastructures.{Nil => FpNil}
import fpinscala.ch3_datastructures.{Cons => fpCons}

class ListTest extends FunSuite {

  test("val x") {
    val x = fpList.x
    println(s"x is [$x].")
  }


  test("tail") {
    val l = fpList(1, 2, 3, 4)
    val t = fpList.tail(l)
    println(t)
  }

  test("setHead") {
    val l = fpList.setHead(FpNil, "Apple")
    println(l)
    assert(l == fpList("Apple"))

    val full = fpList(2, 3, 4, 5)
    val fuller = fpList.setHead(full, 1)
    println(fuller)
    assert(fuller == fpList(1, 2, 3, 4, 5))
  }

  test("drop") {
    val l = fpList(1, 2, 3, 4, 5)

    assert(fpList.drop(l, 0) == fpList(1, 2, 3, 4, 5))
    assert(fpList.drop(l, 1) == fpList(2, 3, 4, 5))
    assert(fpList.drop(l, 2) == fpList(3, 4, 5))
    assert(fpList.drop(l, 3) == fpList(4, 5))
    assert(fpList.drop(l, 4) == fpList(5))
    assert(fpList.drop(l, 5) == FpNil)
    assert(fpList.drop(l, 6) == FpNil)

    println(fpList.drop(l, 0))
    println(fpList.drop(l, 1))
    println(fpList.drop(l, 2))
    println(fpList.drop(l, 3))
    println(fpList.drop(l, 4))
    println(fpList.drop(l, 5))
    println(fpList.drop(l, 6))
    println(fpList.drop(l, 7))
    println(fpList.drop(l, 8))
  }

  test("dropWhile") {
    val l = fpList(1, 2, 3, 4, 5)

    assert(fpList.dropWhile(l, (i: Int) => i < 0) === fpList(1, 2, 3, 4, 5))
    assert(fpList.dropWhile(l, (i: Int) => i < 1) === fpList(1, 2, 3, 4, 5))
    assert(fpList.dropWhile(l, (i: Int) => i <= 1) === fpList(2, 3, 4, 5))
    assert(fpList.dropWhile(l, (i: Int) => i < 2) === fpList(2, 3, 4, 5))
    assert(fpList.dropWhile(l, (i: Int) => i <= 2) === fpList(3, 4, 5))
    assert(fpList.dropWhile(l, (i: Int) => i <= 3) === fpList(4, 5))
    assert(fpList.dropWhile(l, (i: Int) => i < 4) === fpList(4, 5))
    assert(fpList.dropWhile(l, (i: Int) => i <= 4) === fpList(5))

  }

  test("init") {

    //given List(1,2,3,4), init will return List(1,2,3)
    assert(fpList.init(fpList(1, 2, 3, 4, 5)) == fpList(1, 2, 3, 4))
    assert(fpList.init(fpList(1, 2, 3, 4)) == fpList(1, 2, 3))
    assert(fpList.init(fpList(1, 2, 3)) == fpList(1, 2))
    assert(fpList.init(fpList(1, 2)) == fpList(1))
    assert(fpList.init(fpList(1)) == fpList(1))
    assert(fpList.init(FpNil) == FpNil)
  }
}
