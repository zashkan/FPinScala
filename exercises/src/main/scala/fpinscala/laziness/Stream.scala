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

  def takeWhile_via_foldRight(p: A => Boolean): Stream[A] = 
    foldRight(empty[A])((h,t) => if (p(h)) cons(h,t) else empty)

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if (p(h())) => cons(h(), t().takeWhile(p))
    case _ => empty
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

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) => if (p(h)) cons(h,t) else t)  

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h).append(t))

 

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

  //infinite streams:
  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val t: Stream[A] = Cons(() => a, () => t)
    //val c: Stream[A] = cons(a, constant(a))
    t  
  }  

  def from(n: Int): Stream[Int] = {
    //cons(n, from(n+1))
    lazy val t: Stream[Int] = Cons(() => n, () => t.map(_+1))
    t
  }

  def fibs: Stream[Int] = {
    //@annotation.tailrec
    def go(a: Int, b: Int): Stream[Int] =
      cons(a, go(b, a+b))

    go(0, 1)
  }

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

    println("exists:")
    println(c.exists(_ == 2))

    println("forAll:")
    //println(c.forAll(_ < 1))
    c.forAll(_ < 1)
    println(c.forAll_via_foldRight(_ < 1))

    println("combining")
    //println(c.map(_*2).toList)
    //println(c.map(_*2).filter(_%2 == 0).toList)
    c.map(_*2).filter(_%2 == 0).take(5)

    println("************************\ninfinite Streams:")
    println(ones.takeWhile(_>2).toList)
    println(ones.take(2).toList)
    println(ones.exists(_ % 2 != 0))
    println(ones.map(_ + 1).exists(_ == 2))
    
    try {
      //ones.takeWhile(_ == 1).toList
      1/0
    }
    catch { 
      case e: java.lang.Exception => {
        println(e.getMessage())
      } 
    }

    println(ones.forAll(_ != 1))

    println(from(4).take(10).toList)
    println(fibs.take(10).toList)

    println(fpinscala.gettingstarted.MyModule.fib(6))

    for (x <- 1 to 20) {
      if (Stream.fibs.take(x).toList.last==fpinscala.gettingstarted.MyModule.fib(x-1))
        println("match at "+x)
    }

 }
}


































