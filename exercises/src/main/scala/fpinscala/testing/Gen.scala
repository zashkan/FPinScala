package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  def &&(p: Prop): Prop =
    new Prop {
      def check =
        Prop.this.check match {
          case Left(a) => Left(a)
          case Right(b) =>
            p.check match {
              case Left(a) => Left(a)
              case Right(b2) => Right(b + b2)
            }
        }
    }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = ???

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    def betweenStartStop(rng: RNG): (Int, RNG) = {
      val (i, rng2) = rng.nextInt

      if (start <= i && i < stopExclusive) i -> rng2
      else betweenStartStop(rng2)
    }
    val sample = State(betweenStartStop)

    GenST(sample)
  }
}

trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

case class GenST[A](sample: State[RNG, A]) extends Gen[A]

trait SGen[+A] {

}

