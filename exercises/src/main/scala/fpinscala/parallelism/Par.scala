package fpinscala.parallelism

import java.util.concurrent._

object Par {
  /*
  Par[A] needs to be a type that can contain a computation of type A and
  also yield the value when asked for it. It also needs to contain a
  marker for whether its computation is concurrent or sequential. So one
  possible implementation is:

  case class Par[A](comp: A, concurrent: Boolean)

  The above implementation doesn't allow the user to explicitly pass in
  a parallelism strategy, which means it's using a globally-defined
  strategy internally. If we do want to be able to specify a parallelism
  strategy, we need to pass in a java.util.concurrent.ExecutorService
  into each Par value. An implementation which allows this is:

  case class Par[A](comp: A, strategy: ExecutorService)

  With this approach, the 'run' function can use
  'Executor.submit(callable: Callable[T]): Future[T]' to actually run
  the tasks.
  */
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }

  /*
  Par.map2 needs to return Par[SomeType] because it's used in the book
  (p. 100) in a call: Par.map2(sum(l), sum(r))(_ + _) within a function
  (sum) which returns Par[Int].

  In its first parameter list, it needs to take two parameters of
  Par[T1] and Par[T2] because in the above call, it's called with the
  arguments sum(l) and sum(r), which we've already seen must be of type
  Par[SomeType].

  In its second parameter list, it needs to take a function of type (T1,
  T2) => SomeType because it's called with the argument '_ + _' which
  takes two arguments and returns SomeType.

  So, replacing

    - T1 with A,

    - T2 with B, and

    - SomeType with C,

  We can infer the actual type to be as follows:
  */
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es) 
      val bf = b(es)

      new Future[C] {
        def isDone = af.isDone && bf.isDone

        def get(timeout: Long, units: TimeUnit) = {
          val startTime = System.currentTimeMillis
          val a = af.get(timeout, units)
          val elapsedTime = System.currentTimeMillis - startTime
          val b =
            bf.get(
              /*
              Give the second future less time to complete because the
              first future took some time.
              */
              TimeUnit.MILLISECONDS.convert(timeout, units) - elapsedTime,
              TimeUnit.MILLISECONDS
            )

          f(a, b)
        }

        def get = get(Long.MaxValue, TimeUnit.MILLISECONDS)

        def isCancelled = af.isCancelled || bf.isCancelled

        def cancel(evenIfRunning: Boolean) =
          af.cancel(evenIfRunning) && bf.cancel(evenIfRunning)
      }
    }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] { 
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List.empty[A])) { (pa, pbs) =>
      map2(pa, pbs) { (a, bs) => a :: bs }
    }

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
    fork(sequence(as.map(asyncF(f))))

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    fork(
      map(
        sequence(as.map(asyncF(a => if (f(a)) List(a) else List.empty)))
      ) {
        _.flatten
      }
    )

  def parFold[A, B](as: IndexedSeq[A], z: B)(project: A => B)(merge: (B, B) => B): Par[B] = {
    import scala.util.control.TailCalls._

    def go(as: IndexedSeq[A]): TailRec[Par[B]] = {
      val asLength = as.length

      if (asLength < 1) done(unit(z))
      else if (asLength == 1) done(unit(project(as.head)))
      else {
        val (l, r) = as.splitAt(as.length / 2)

        for {
          lFold <- tailcall(go(l))
          rFold <- tailcall(go(r))
        } yield fork(map2(lFold, rFold)(merge))
      }
    }

    fork(go(as).result)
  }

  def wordCount(paras: List[String]): Par[Int] =
    parFold(paras.toIndexedSeq, 0)(_.split(' ').length)(_ + _)

  def map3
    [A, B, C, D]
    (pa: Par[A], pb: Par[B], pc: Par[C])
    (f: (A, B, C) => D):
    Par[D] =
    map2(
      map2(pa, pb) { (a, b) => a -> b },
      pc
    ) { case ((a, b), c) => f(a, b, c) }

  def map4
    [A, B, C, D, E]
    (pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D])
    (f: (A, B, C, D) => E):
    Par[E] =
    map2(
      map2(
        map2(pa, pb) { (a, b) => a -> b },
        pc
      ) { (ab, c) => ab -> c },
      pd
    ) { case (((a, b), c), d) => f(a, b, c, d) }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  /*
  # Exercise 7.7

  Given map(y)(id) == y, .......................... (1)

  Prove: map(map(y)(g))(f) == map(y)(f compose g) . (2)

  If (2) holds, then

    map(map(y)(g))(id) == map(y)(id compose g)

  Applying (1):

    map(y)(g) == map(y)(g)

  Therefore proven. Also,

    map(map(y)(id))(f) == map(y)(f compose id)

  Applying (1):

    map(y)(f) == map(y)(f)

  Therefore proven (again).

  # Exercise 7.9

  Given a thread pool of size n, we can create a deadlock with this
  implementation of 'fork' by wrapping up n + 1 consecutive calls to
  'fork', which boil down to n + 1 consecutive (and blocking) calls to
  'ExecutorService#submit'. Each call asks the 'ExecutorService' for a
  new thread to run in, but there are only n threads, so the last call
  will wait forever (deadlocked) for a new thread.
  */

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => 
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
