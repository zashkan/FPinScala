package fpinscala.laziness

import Stream._
trait Stream[+A] {
  def toList: List[A] = foldRight(List.empty[A]) { (a, b) => a :: b }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(x: Stream[A], n: Int, y: Stream[A]): Stream[A] =
      x match {
        case Empty => y
        case Cons(h, t) =>
          if (n < 1) y else go(t(), n - 1, Cons(h, () => y))
      }

    go(this, n, Empty)
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] =
    this match {
      case Empty => Empty
      case Cons(h, t) =>
        val tForced = t()
        if (n < 2) tForced else tForced.drop(n - 1)
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A]) { (a, b) =>
      if (p(a)) Cons(() => a, () => b) else Empty
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true) { (a, b) => p(a) && b }

  def headOption: Option[A] =
    foldRight(Option.empty[A]) { (a, _) => Some(a) }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B]) { (a, b) =>
      f(a).foldRight(b) { (a, b) => Cons(() => a, () => b) }
    }

  def map[B](f: A => B): Stream[B] =
    flatMap { a => Cons(() => f(a), () => Empty) }

  def filter(p: A => Boolean): Stream[A] =
    flatMap { a => if(p(a)) Cons(() => a, () => Empty) else Empty }

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s) { (a, b) => Cons(() => a, () => b) }

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}

