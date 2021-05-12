package fpinscala.ch2_gettingstarted

import org.scalatest.FunSuite

class GettingStartedAnswersTest extends FunSuite {

  test("fib") {
    assert(MyModule.fib(0) == 0)
    assert(MyModule.fib(1) == 1)
    assert(MyModule.fib(2) == 1)
    assert(MyModule.fib(3) == 2)
    assert(MyModule.fib(4) == 3)
    assert(MyModule.fib(5) == 5)
  }

  test("isSorted numberz") {
    val sorter = (x: Int, y: Int) => x > y
    assert(PolymorphicFunctions.isSorted(Array(1, 2, 3, 4), sorter))
    assert(!PolymorphicFunctions.isSorted(Array(4, 1, 3, 2), sorter))
  }

  test("isSorted letter") {
    val sorter = (x: String, y: String) => x > y
    assert(PolymorphicFunctions.isSorted(Array("Apple", "Bog", "Cat", "Dog"), sorter))
    assert(PolymorphicFunctions.isSorted(Array("Dog", "Cat", "Bog", "Apple"), (x: String, y: String) => x < y))
    assert(!PolymorphicFunctions.isSorted(Array("Dog", "Apple", "Cat", "Bog"), sorter))
  }

}
