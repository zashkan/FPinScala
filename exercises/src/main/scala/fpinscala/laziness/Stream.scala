package fpinscala.laziness

import Stream._
trait Stream[+A] {

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

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h,t) => h() :: t().toList
  }

  def toList2: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if (n > 0) => cons(h(), t().take(n-1))
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h,t) if (n > 0) => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if (p(h())) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = sys.error("todo")

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h,t) => Some(h())
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

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
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")

  def myPrint() = println("from my Stream")


}

object myObj {
  def main(args: Array[String]) = { 
    println("in Stream!")
    Stream.myPrint

    def item1(): Int = { println("item 1!"); 1}
    def item2(): Int = { println("item 2!"); 2}
    def item3(): Int = { println("item 3!"); 3}

    //val c = Stream(item1,item2,item3)
    //val c = Cons(() => item1(), () => Stream.empty)
    val c = Cons(() => item1, 
                 () => Cons(() => item2, 
                            () => Cons(() => item3,
                                       () => Stream.empty)))
    
    val h1 = c.headOption
    val h2 = c.headOption

    val l = c.toList
    println(l)

    val l2 = c.toList2
    println(l2)

    println("take(n):")
    val sTake = c.take(2)
    println(sTake.toList)

    println("drop(n):")
    val sDrop = c.drop(1)
    println(sDrop.toList)

    println("takeWhile(p) first call:\n" + c.takeWhile(_ < 2).toList)

    println("takeWhile(p) second call:")
    val t = c.takeWhile(_ < 2)
    println(t.toList)
 }
}


































