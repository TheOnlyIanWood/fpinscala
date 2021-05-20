package fpinscala.ch3_datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  //
  //  def size[A](t: Tree[A]): Int = {
  //
  //    def go[A](t: Tree[A], leavesAcc: Int, leftAcc: Int, rightAcc: Int): Int = t match {
  //      case Leaf(_) => leavesAcc + leftAcc + rightAcc
  //      case Branch(left, right) => go(left, leavesAcc, (leftAcc + 1), rightAcc) + go(right, leavesAcc, leftAcc, rightAcc + 1)
  //    }
  //
  //    go(t, 0, 0, 0) + 1
  //
  //
  //  }


  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }


}