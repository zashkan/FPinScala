package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    // case None => None
    // case Some(a) => f(a)
    map(f) getOrElse None
  }
    
  def orElse[B>:A](ob: => Option[B]): Option[B] = 
    //case None => ob
    //case _ => this
    map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = 
    //case Some(v) if f(v) => this
    //case _ => None
    flatMap(a => if (f(a)) Some(a) else None)  
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
  val absO: Option[Double] => Option[Double] = lift(math.abs)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x-m, 2))))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
  // (a,b) match {
  //   case (None,_) => None
  //   case (_,None) => None
  //   case (Some(x),Some(y)) => Some(f(x,y))
    a flatMap (aa => b map (bb => f(aa, bb)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = sys.error("todo")

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = 
    a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None}

  def retOpt(a: Int): Option[Int] =
    if (a <= 0) None
    else Some(a)

  def double(a: Int): Int = 2 * a
}

object myModule {
  def main(Args: Array[String]): Unit = {
    println("hello from Option")
    //Option.failingFn(3)
    println(math.abs(-2))

    import Option._
    val x: Option[Double] = Some(-2)
    println(absO(x))
    println(absO(None))

    Option.failingFn2(3)

    val names = List("My", "Words", null, "aRE")
    //println(names.map(_.toLowerCase))
    val namesOpt = names.map(Some(_))
    println(namesOpt)

    val ages = List("1", "2", "4", "")
    println(Option.traverse(ages)(x => Try(x.toInt)))
    //namesOpt.flatMap(x => x)
    println(Either.Try(5/5))
    println(Either.safeDiv(5, 0))
    val e = Either.Try(0/0)
    println(e)

    val y: Option[Int] = Some(5)
    println("map with Int => Option:" + y.map(retOpt _))
    println("map with Int => Int:" + None.map(double _))

    println("flatMap with x => Option: " + y.flatMap(retOpt _))

    println(Either.mkName(""))
    println(Either.mkAge(1))
    println(Either.mkPerson("d", 1))

    println(Either.mkListErr(-2,-1).map(_+10))
  } 
}



















