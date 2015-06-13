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

  def take_via_unfold(n: Int): Stream[A] =
    unfold((this,n)) {
      case (Cons(h,t),n) if (n > 0) => Some((h(), (t(),n-1)))
      case _ => None
    }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h,t) if (n > 0) => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if (p(h())) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhile_via_foldRight(p: A => Boolean): Stream[A] = 
    foldRight(empty[A])((h,t) => if (p(h)) cons(h,t) else empty)
  
  def takeWhile_via_unfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h,t) if (p(h())) => Some((h(), t()))
      case _ => None
    }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def forAll_via_foldRight(p: A => Boolean): Boolean = 
    foldRight(true)((h,t) => p(h) && t)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h,t) => Some(h())
  }

  def headOption_via_foldRight: Option[A] =
    foldRight(None: Option[A])((h,_) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h,t) => cons(f(h),t))

  def map_via_unfold[B](f: A => B): Stream[B] =
     unfold(this) {
    case Cons(h,t) => Some((f(h()), t()))
    case empty => None
    }  

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) => if (p(h)) cons(h,t) else t)  

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h).append(t))

  def zipWith[B,C](b: Stream[B])(f: (A, B) => C): Stream[C] = 
    unfold((this,b)) {
      case (Cons(h1,t1), Cons(h2,t2)) => Some((f(h1(),h2()), (t1(),t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold(this, s2) {
      case (Cons(h1,t1), Cons(h2,t2)) => Some(((Some(h1()) -> Some(h2())), (t1() -> t2())))
      case (Cons(h1,t1), _) => Some(((Some(h1()) -> Option.empty[B]),(t1() -> empty[B])))
      case (_, Cons(h2,t2)) => Some(((Option.empty[A] -> Some(h2())) , (empty[A] -> t2())))
      case _ => None
    }

  // def startsWith[A](s: Stream[A]): Boolean = (this,s) match {
  //   case (_,Empty) => true
  //   case (Empty,_) => false
  //   case (Cons(ha,ta),Cons(hb,tb)) if (ha() == hb()) => ta().startsWith(tb())
  //   case (_,_) => false
  // }

  def startsWith[A](s: Stream[A]): Boolean = 
    this.zipAll(s).takeWhile(x => x._2 != None).forAll(x => x match { case (h1,h2) => h1==h2})

  def tails: Stream[Stream[A]] = 
    unfold(this){
      case Cons(h,t) => Some((Cons(h,t),t()))
      case _ => None 
    }.append(Stream(empty))

  def hasSubsequence[A](s: Stream[A]): Boolean = 
    tails exists (_ startsWith s)

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

  //infinite streams:
  val ones: Stream[Int] = Stream.cons(1, ones)

  val ones_via_unfold: Stream[Int] = unfold(1)(s => Some(s,s))

  def constant[A](a: A): Stream[A] = {
    lazy val t: Stream[A] = Cons(() => a, () => t)
    t  
    //cons(a, constant(a))
  }  

  def constant_via_unfold[A](a: A): Stream[A] = 
    unfold(a)(s => Some(s,s))

  def from(n: Int): Stream[Int] = {
    //cons(n, from(n+1))
    lazy val t: Stream[Int] = Cons(() => n, () => t.map(_+1))
    t
  }

  def from_via_unfold(n: Int): Stream[Int] =
    unfold(n)(s => Some(s,s+1))

  def fibs: Stream[Int] = {
    //@annotation.tailrec
    def go(a: Int, b: Int): Stream[Int] =
      cons(a, go(b, a+b))

    go(0, 1)
  }

  def fibs_via_unfold: Stream[Int] = 
    //unfold((0,1))(s => Some(s._1, (s._2, s._1+s._2)))
    unfold(0,1){ case (a,b) => Some(a, (b, a+b))}

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

}