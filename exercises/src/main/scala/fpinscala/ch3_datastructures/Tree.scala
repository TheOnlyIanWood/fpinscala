package fpinscala.ch3_datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + depth(left) max depth(right)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](t: Tree[A])(leafFun: => A => B)(op: (B, B) => B): B = t match {
    case Leaf(value) => leafFun(value)
    case Branch(left, right) => op(fold(left)(leafFun)(op), fold(right)(leafFun)(op))
  }


  def sizeFold[A, B](t: Tree[A]): Int = {
    fold(t)(_ => 1)(1 + _ + _)
  }

  def maximumFold(t: Tree[Int]): Int = {
    fold[Int, Int](t)((i: Int) => i)(_ max _)
  }

  def depthFold(t: Tree[Int]): Int = {
    fold[Int, Int](t)(_ => 1)(1 + _ max _)
  }

}