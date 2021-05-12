fpinscala.ch3_datastructures.List

import fpinscala.ch3_datastructures.List.foldRight
import fpinscala.ch3_datastructures.{Cons => FpCons, List => FpList, Nil => FpNil}

def product2(ns: FpList[Double]) = foldRight(ns, 1.0)(_ * _)


val l = FpList(1.0, 2.0, 3.0, 4.0, 5.0)
product2(l)


def product3(ns: FpList[Double]) = {
  foldRight(ns, 1.0)((x, y) => {
    if (x == 0.0 || y == 0.0) 0.0
    else x * y
  })
}


product3(l)

foldRight(FpList(1, 2, 3), FpNil: FpList[Int])(FpCons(_, _))

