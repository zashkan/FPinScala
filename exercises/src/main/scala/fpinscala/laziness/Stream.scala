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

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if (n > 0) => Cons(h, () => t().take(n - 1))
      case _ => Empty
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

  def startsWith[B >: A](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(_._2 != None).forAll { os => os._1 == os._2 }

  def tails: Stream[Stream[A]] =
    unfold(this) { s =>
      s match {
        case Cons(h, t) => Option(s -> t())
        case _ => None
      }
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold(this -> s2) {
      case (Cons(ha, ta), Cons(hb, tb)) =>
        Option(Option(ha()) -> Option(hb()) -> (ta() -> tb()))
      case (Cons(ha, ta), Empty) =>
        Option(Option(ha()) -> None -> (ta() -> Empty))
      case (Empty, Cons(hb, tb)) =>
        Option(None -> Option(hb()) -> (Empty -> tb()))
      case (Empty, Empty) => None

    }
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

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = cons(a, go(b, a + b))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    val emptyA: Stream[A] = empty

    def go(s: S, a: A): Stream[A] =
      cons(
        a,
        f(s).fold(emptyA) { nextValState =>
          go(nextValState._2, nextValState._1)
        }
      )

    f(z).fold(emptyA) { nextValState =>
      go(nextValState._2, nextValState._1)
    }
  }

  def fibs_unfold: Stream[Int] =
    unfold((0, 1)) { s => Option((s._1, (s._2, s._1 + s._2))) }

  def from_unfold(n: Int): Stream[Int] =
    unfold(n) { s => Option((s, s + 1)) }

  def constant_unfold[A](a: A): Stream[A] =
    unfold(a) { s => Option((s, s)) }

  val ones_unfold: Stream[Int] = unfold(1) { s => Option((s, s)) }

  def map_unfold[A, B](as: Stream[A])(f: A => B): Stream[B] =
    unfold(as) {
      case Cons(h, t) => Option(f(h()) -> t())
      case Empty => None
    }

  def take_unfold[A](as: Stream[A], n: Int): Stream[A] =
    unfold(as -> n) {
      case (Cons(h, t), n) if (n > 0) => Option(h() -> (t(), n - 1))
      case _ => None
    }

  def takeWhile_unfold[A](as: Stream[A], p: A => Boolean): Stream[A] =
    unfold(as) {
      case Cons(h, t) =>
        val hForced = h()
        if (p(hForced)) Option(hForced -> t()) else None
      case _ => None
    }

  def zipWith[A, B, C](as: Stream[A], bs: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold(as -> bs) {
      case (Cons(ha, ta), Cons(hb, tb)) =>
        Option(f(ha(), hb()) -> (ta() -> tb()))
      case _ => None
    }
}

