package fpinscala.ch3_datastructures

import fpinscala.ch3_datastructures.List.tail

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`

case object Nil extends List[Nothing] // A `List` data constructor representing the empty list

/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = { // Utility functions{
    println(s"z [$z]")

    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // exercises begin
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, _) => Cons(h, l)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => {
      if (n == 0) l
      else if (n == 1) t
      else drop(t, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  def init[A](l: List[A]): List[A] = {

    @tailrec
    def dropTheLast[A](xs: List[A], acc: List[A]): List[A] = xs match {
      case Nil => println(s"1:"); acc //fucksake. Need uppercase for matching
      case Cons(h, Cons(h1, Nil)) => println(s"3: $h $h1"); append(List(h), acc)
      case Cons(h, t) => println(s"4: $h $t"); dropTheLast(t, setHead(acc, h))
    }

    val reversedLoop = dropTheLast(l, Nil)

    @tailrec
    def reverse[A](xs: List[A], acc: List[A]): List[A] = xs match {
      case Nil => acc
      case Cons(h, t) => reverse(t, setHead(acc, h))
    }

    reverse(reversedLoop, Nil)
  }

  def length[A](l: List[A]): Int = {

    def counter(a: A, b: Int): Int = {
      println(s"a [$a] [$b]")
      b + 1
    }

    //    foldRight(l, 0)(counter)
    foldRight(l, 0)((a, b) => b + 1)
  }

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    println(s"z [$z]")
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  }

  def sumLeft(xs: List[Int]) = foldLeft(xs, 0)((x, y) => x + y)

  def productLeft(xs: List[Int]) = foldLeft(xs, 1)((x, y) => x * y)

  def lengthLeft[A](xs: List[A]): Int = foldLeft(xs, 0)((b, a) => b + 1)

  def reverse[A](l: List[A]) = {

    @tailrec
    def reverse[A](xs: List[A], acc: List[A]): List[A] = xs match {
      case Nil => acc
      case Cons(h, t) => reverse(t, setHead(acc, h))
    }

    reverse(l, Nil)
  }

  def reverseLeft[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))
  }

  def appendFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((x, y) => Cons(x, y))
  }

  //This may be safer as stack safe
  def appendFoldLeft[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(reverseLeft(a1), a2)((x, y) => Cons(y, x))
  }

  def flatten[A, B](xs: List[List[B]]): List[B] = {

    //TODO This isn't linear I expect, especially due to the reverse at the end.
    def outerListLoop(xs: List[List[B]], acc: List[B]): List[B] = xs match {
      case Nil => acc
      case Cons(headA, tailA) => headA match {
        case Nil => acc
        case Cons(headB, tailB) => {

          def eachListLoop(bs: List[B], acc: List[B]): List[B] = bs match {
            case Nil => outerListLoop(tailA, acc)
            case Cons(headC, tailC) => eachListLoop(tailC, setHead(acc, headC))
          }

          eachListLoop(tailB, setHead(acc, headB))
        }
      }
    }

    reverseLeft(outerListLoop(xs, Nil: List[B]))
  }

  def add1(ints: List[Int]): List[Int] = {
    foldRight(ints, Nil: List[Int])((x, y) => Cons(x + 1, y))
  }

  def doublesToString(doubles: List[Double]): List[String] = {
    foldRight(doubles, Nil: List[String])((x, y) => Cons(x.toString, y))
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((x, y) => Cons(f(x), y))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {

    @tailrec
    def loop(as: List[A], acc: List[A]): List[A] = as match {
      case Nil => acc
      case Cons(h, t) => if (f(h)) loop(t, setHead(acc, h)) else loop(t, acc)
    }

    reverse(loop(l, Nil: List[A]))
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = flatten(map(l)(f))


}
