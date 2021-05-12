fpinscala.ch3_datastructures.List.x

import fpinscala.ch3_datastructures.{Cons => fpCons, List => FpList, Nil => FpNil}

val s = ""
val l = FpList(1, 2, 3, 4, 5)
//val l = fpList(1, 2)


def loop[A](xs: FpList[A], acc: FpList[A]): FpList[A] = xs match {
  case FpNil => println(s"1:"); acc //fucksake. Need uppercase for matching
  case fpCons(h, fpCons(h1, fpCons(h2, FpNil))) => println(s"2: $h $h1 $h2"); FpList.append(FpList(h1, h), acc)
  case fpCons(h, t) => println(s"3: $h $t"); loop(t, FpList.setHead(acc, h))
}

val reversedloop = loop(l, FpNil)

def reverse[A](xs: FpList[A], acc: FpList[A]): FpList[A] = {
  xs match {
    case FpNil => acc
    case fpCons(h, t) => reverse(t, FpList.setHead(acc, h))
  }
}

reverse(reversedloop, FpNil)