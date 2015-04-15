package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(a) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(a) => a
      case Branch(left, right) => maximum(left).max(maximum(right))
    }

  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(a) => 0
      case Branch(left, right) =>
        (depth(left) + 1).max(depth(right) + 1)
    }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
}

