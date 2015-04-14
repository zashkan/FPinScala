package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  /*
  This implementation of foldRight is not tail-recursive because the
  recursive call is not in tail position.
  */
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  /*
  Product2, implemented in terms of foldRight, can't immediately halt
  recursion on encountering a 0.0 because foldRight doesn't have any
  logic for short-circuiting a recursive traversal of a list.

  For short-circuiting to work, we would need to redefine foldRight to
  pass it a 'sentinel' value; if it encounters the sentinel value in the
  list, it would short-circuit and immediately return the accumulated
  value.
  */
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => ???
      case Cons(_, t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => List(h)
      case Cons(_, t) => Cons(h, t)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n < 1) l else drop(tail(l), n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) =>
        if (f(h)) dropWhile(t, f)
        else l
    }

  /*
  init can't be implemented in constant time because it needs to
  traverse the entire list in order to find all elements but the last.
  */
  def init[A](l: List[A]): List[A] = {
    def reverse(l: List[A]): List[A] = {
      def go(l: List[A], accum: List[A]): List[A] =
        l match {
          case Nil => accum
          case Cons(h, t) => go(t, Cons(h, accum))
        }

      go(l, Nil)
    }

    def go(l: List[A], accum: List[A]): List[A] =
      l match {
        case Nil => ???
        case Cons(h, Nil) => Nil
        case Cons(h1, Cons(h2, Nil)) => Cons(h1, accum)
        case Cons(h, t) => go(t, Cons(h, accum))
      }

    reverse(go(l, Nil))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0) { (x, z) => z + 1 }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum3(xs: List[Int]): Int = foldLeft(xs, 0)(_ + _)

  def product3(xs: List[Int]): Int = foldLeft(xs, 1)(_ * _)

  def length3(xs: List[Int]): Int = foldLeft(xs, 0) { (z, x) => z + 1 }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A]) { (b, a) => Cons(a, b) }

  def foldLeft_using_foldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    def foldF(l: List[A]) =
      foldRight(l, (b: B) => b) {
        (a: A, fz: B => B) => (b: B) => fz(f(b, a))
      }

    l match {
      case Nil => z
      case Cons(x, xs) => foldF(xs)(f(z, x))
    }
  }

  def foldRight_using_foldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z) { (b, a) => f(a, b) }

  def append_using_foldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2) { (a: A, as: List[A]) => Cons(a, append(as, a2)) }

  def concat[A](xss: List[List[A]]): List[A] =
    foldRight_using_foldLeft(xss, Nil: List[A])(append(_, _))

  def add1(xs: List[Int]): List[Int] =
    foldRight_using_foldLeft(xs, Nil: List[Int]) {
      (x: Int, xs: List[Int]) => Cons(x + 1, xs)
    }

  def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")
}

