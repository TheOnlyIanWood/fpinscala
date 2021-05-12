package fpinsccala.ch3_datastructures

import org.scalatest.FunSuite

import fpinscala.ch3_datastructures.{List => fpList}

class ListTest extends FunSuite{

  test("val x"){
    val x = fpList.x
    println(s"x is [$x].")
  }


  test("tail"){
    val l = fpList(1,2,3,4)
    val t  = fpList.tail(l)
    println(t)
  }
}
