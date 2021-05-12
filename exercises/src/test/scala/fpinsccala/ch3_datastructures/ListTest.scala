package fpinsccala.ch3_datastructures

import fpinscala.ch3_datastructures.List.foldRight
import org.scalatest.FunSuite
import fpinscala.ch3_datastructures.{List => FpList}
import fpinscala.ch3_datastructures.{Nil => FpNil}
import fpinscala.ch3_datastructures.{Cons => FpCons}

class ListTest extends FunSuite {

  test("val x") {
    val x = FpList.x
    println(s"x is [$x].")
  }


  test("tail") {
    val l = FpList(1, 2, 3, 4)
    val t = FpList.tail(l)
    println(t)
  }

  test("setHead") {
    val l = FpList.setHead(FpNil, "Apple")
    println(l)
    assert(l == FpList("Apple"))

    val full = FpList(2, 3, 4, 5)
    val fuller = FpList.setHead(full, 1)
    println(fuller)
    assert(fuller == FpList(1, 2, 3, 4, 5))
  }

  test("drop") {
    val l = FpList(1, 2, 3, 4, 5)

    assert(FpList.drop(l, 0) == FpList(1, 2, 3, 4, 5))
    assert(FpList.drop(l, 1) == FpList(2, 3, 4, 5))
    assert(FpList.drop(l, 2) == FpList(3, 4, 5))
    assert(FpList.drop(l, 3) == FpList(4, 5))
    assert(FpList.drop(l, 4) == FpList(5))
    assert(FpList.drop(l, 5) == FpNil)
    assert(FpList.drop(l, 6) == FpNil)

    println(FpList.drop(l, 0))
    println(FpList.drop(l, 1))
    println(FpList.drop(l, 2))
    println(FpList.drop(l, 3))
    println(FpList.drop(l, 4))
    println(FpList.drop(l, 5))
    println(FpList.drop(l, 6))
    println(FpList.drop(l, 7))
    println(FpList.drop(l, 8))
  }

  test("dropWhile") {
    val l = FpList(1, 2, 3, 4, 5)

    assert(FpList.dropWhile(l, (i: Int) => i < 0) === FpList(1, 2, 3, 4, 5))
    assert(FpList.dropWhile(l, (i: Int) => i < 1) === FpList(1, 2, 3, 4, 5))
    assert(FpList.dropWhile(l, (i: Int) => i <= 1) === FpList(2, 3, 4, 5))
    assert(FpList.dropWhile(l, (i: Int) => i < 2) === FpList(2, 3, 4, 5))
    assert(FpList.dropWhile(l, (i: Int) => i <= 2) === FpList(3, 4, 5))
    assert(FpList.dropWhile(l, (i: Int) => i <= 3) === FpList(4, 5))
    assert(FpList.dropWhile(l, (i: Int) => i < 4) === FpList(4, 5))
    assert(FpList.dropWhile(l, (i: Int) => i <= 4) === FpList(5))

    // with two param lists don't need to have type for anon fun
    assert(FpList.dropWhile2(l)(i => i < 0) === FpList(1, 2, 3, 4, 5))
    assert(FpList.dropWhile2(l)(i => i < 1) === FpList(1, 2, 3, 4, 5))
    assert(FpList.dropWhile2(l)(i => i <= 1) === FpList(2, 3, 4, 5))
    assert(FpList.dropWhile2(l)(i => i < 2) === FpList(2, 3, 4, 5))
    assert(FpList.dropWhile2(l)(i => i <= 2) === FpList(3, 4, 5))
    assert(FpList.dropWhile2(l)(i => i <= 3) === FpList(4, 5))
    assert(FpList.dropWhile2(l)(i => i < 4) === FpList(4, 5))
    assert(FpList.dropWhile2(l)(i => i <= 4) === FpList(5))


  }

  test("init") {

    //given List(1,2,3,4), init will return List(1,2,3)
    assert(FpList.init(FpList(1, 2, 3, 4, 5)) == FpList(1, 2, 3, 4))
    assert(FpList.init(FpList(1, 2, 3, 4)) == FpList(1, 2, 3))
    assert(FpList.init(FpList(1, 2, 3)) == FpList(1, 2))
    assert(FpList.init(FpList(1, 2)) == FpList(1))
    assert(FpList.init(FpList(1)) == FpList(1))
    assert(FpList.init(FpNil) == FpNil)
  }

  test("EX 7 product") {

    //I don't think it can short circuit
    val l = FpList(1.0, 2.0, 0.0, 4.0, 5.0)
    val d = FpList.product(l)
    println(d)
  }

  test("EX 8 Nil and Cons") {
    foldRight(FpList(1, 2, 3), FpNil: FpList[Int])(FpCons(_, _))
    //cons is right assoiactive?
  }

  test("EX 9 Length of a list") {
    val l = FpList(1, 2, 3, 4)

    assert(FpList.length(l) == 4)
  }

  test("foldLeft") {
    val l = FpList(1, 2, 3)

    assert(FpList.foldLeft(l, 0)((x, y) => x + y) == 6)
    assert(FpList.foldRight(l, 0)((x, y) => x + y) == 6)

    val a = FpList(1000, 100, 10)
    println(FpList.foldLeft(a, 0)(_ - _))
    println(FpList.foldRight(a, 0)(_ - _))

  }
}
