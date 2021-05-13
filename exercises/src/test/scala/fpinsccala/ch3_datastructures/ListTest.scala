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

  test("EX10 foldLeft") {
    val l = FpList(1, 2, 3, 4)

    assert(FpList.foldLeft(l, 0)((x, y) => x + y) == 10)
    assert(FpList.foldRight(l, 0)((x, y) => x + y) == 10)

    val a = FpList(1000, 100, 10)
    println(FpList.foldLeft(a, 0)(_ - _))
    println(FpList.foldRight(a, 0)(_ - _))

  }

  test("EX11 sum, product and length using foldLeft") {
    val l = FpList(1, 2, 3, 4)

    assert(FpList.sumLeft(l) == 10)
    assert(FpList.productLeft(l) == 24)
    assert(FpList.lengthLeft(l) == 4)
  }

  test("EX12 reverse") {
    val l = FpList(1, 2, 3, 4)
    assert(FpList.reverse(l) == FpList(4, 3, 2, 1))
    assert(FpList.reverseLeft(l) == FpList(4, 3, 2, 1))
  }

  test("EX13 HARD foldLeft in terms of foldRight") {
    (pending)
  }

  test("EX14 append using a fold") {
    val a1 = FpList(1, 2, 3, 4)
    val a2 = FpList(5, 6, 7, 8)

    assert(FpList.appendFoldRight(a1, a2) == FpList(1, 2, 3, 4, 5, 6, 7, 8))
    assert(FpList.appendFoldLeft(a1, a2) == FpList(1, 2, 3, 4, 5, 6, 7, 8))
  }

  test("EX15 HARD  Write a function that concatenates a list of lists into a single list. ") {

    val l = FpList(FpList(1, 2, 3), FpList(4, 5, 6), FpList(7, 8, 9))
    val expected = FpList(1, 2, 3, 4, 5, 6, 7, 8, 9)
    assert(FpList.flatten(l) == expected) //TODO check this impl for a Nil case
  }

  test("EX16 transforms a list of integers by adding 1to each element") {
    assert(FpList.add1(FpList(1, 2, 3)) == FpList(2, 3, 4))
  }

  test("EX17 each value in a List[Double] into a String.") {
    assert(FpList.doublesToString(FpList(1.0, 1.1, 1.2)) == FpList("1.0", "1.1", "1.2"))
  }

  test("EX18 map") {
    assert(FpList.map(FpList(1, 2, 3))(_ + 1) == FpList(2, 3, 4))
    assert(FpList.map(FpList(1.0, 1.1, 1.2))(_.toString) == FpList("1.0", "1.1", "1.2"))
  }

  test("EX19 filter") {
    val input = FpList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val expect = FpList(2, 4, 6, 8, 10)
    assert(FpList.filter(input)(_ % 2 == 0) == expect)
  }

  test("EX20 flatMap") {
    //flatMap(List(1,2,3))(i => List(i,i)) ==  List(1,1,2,2,3,3)
    assert(FpList.flatMap(FpList(1, 2, 3))(i => FpList(i, i)) == FpList(1, 1, 2, 2, 3, 3))
  }

  test("EX21 filterWithFlatMap") {
    val input = FpList(2, 3, 4, 5, 6, 7, 8, 9, 10)
    val expect = FpList(2, 4, 6, 8, 10)
    //    assert(FpList.filterWithFlatMap(input)(_ % 2 == 0) == expect)
    //TODO I think my impl of flatten is the problem.
    assert(FpList.flatten(FpList.map(input)(i => if (i % 2 != 0) FpList(FpNil) else FpList(FpList(i)))) == expect)

  }

  test("EX22 accepts two lists and constructs a new list by adding corresponding element ") {
    val a = FpList(1, 2, 3)
    val b = FpList(4, 5, 6)
    val expected = FpList(5, 7, 9)
    assert(FpList.addTwoLists(a, b) == expected)
  }

  test("EX23 zipWith.") {
    val a = FpList(1, 2, 3)
    val b = FpList(4, 5, 6)
    val expected = FpList(5, 7, 9)
    assert(FpList.zipWith(a, b)(_ + _) == expected)
  }
}
