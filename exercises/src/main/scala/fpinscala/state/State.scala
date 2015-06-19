package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map_via_flatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    ((i.toDouble)/(Int.MaxValue.toDouble+1), r)
  }

  def double_via_map: Rand[Double] =
    map(nonNegativeInt)(i => (i.toDouble)/(Int.MaxValue.toDouble+1))

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i,d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d), rng2) = intDouble(rng)
    ((d,i), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1,d2,d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(c: Int, acc: List[Int])(rng: RNG): (List[Int], RNG) = {
      if (c < 1) (acc, rng)
      else {
        val (i, r) = rng.nextInt
        go(c-1, i::acc)(r)
      }
    }
    go(count, List())(rng)
  }

  def ints_via_sequence(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    rng => {
      val (a, rngA) = ra(rng)
      val (b, rngB) = rb(rngA)
      (f(a, b), rngB)
    }

  def map2_via_flatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    //flatMap(ra)(a => f(a,rb(r)))
    flatMap(ra)(a => map(rb)(b => f(a,b)))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
    //fs.foldRight(unit(List[A]()))((h,t) => map2(h, t)(_ :: _))
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => if (i-i%n+n-1 >= 0) unit(i%n) else nonNegativeLessThan(n))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = 
    rng => {
      val (a, r2) = f(rng)
      val (b, r3) = g(a)(r2)
      (b, r3)
    }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = 
    // State { (s: S) =>
    //   val (a, s2) = run(s)
    //   (f(a), s2)
    // }
    //flatMap(a => State.unit(f(a)))
    flatMap(f andThen State.unit)
    
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    // State { (s: S) =>
    //   val (a, s2) = run(s)
    //   val (b, s3) = sb.run(s2)
    //   (f(a,b), s3)
    // }
    flatMap(a => sb.map(b => f(a,b)))
    
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, s2) = run(s)
      val (b, s3) = f(a).run(s2)
      (b, s3)
    }

}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = 
    State(a -> _)

  def sequence[S, A](fs: List[State[S,A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List[A]()))((h, acc) => h.map2(acc)(_ :: _))
  
  import fpinscala.applicative.StateUtil.{ get, set }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  //def get[S]: State[S, S] = State(s => (s, s))
  //def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object myModule {
  def main(args: Array[String]) = {
    println("from state!")
    var rng = RNG.Simple(42)
    println(rng)
    println(rng.nextInt)
    println(rng.nextInt)

    println(RNG.nonNegativeInt(rng))

    println(Int.MaxValue, Int.MinValue)
    val (i, rng2) = RNG.double(rng)

    println("==========================")
    println("double:")
    println(RNG.double(rng2))
    println(RNG.double_via_map(rng2))

    println("==========================") 

    println(rng2.nextInt)
    println(RNG.intDouble(rng2))
    println(RNG.doubleInt(rng2))
    println(RNG.double3(rng2))

    println(RNG.ints(3)(rng2))
    println(RNG.ints_via_sequence(3)(rng2))

    println("==========================")   
    //println(RNG.map(RNG.unit(5))(_)(rng2))
    println(RNG.unit(5)(rng2))
    println(RNG.nonNegativeLessThan(6)(rng2))
  }
}