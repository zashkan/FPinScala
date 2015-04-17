package fpinscala.errorhandling

import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(_) => this
      case Left(_) => b
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap { aVal => b.map { bVal => f(aVal, bVal) } }

  /*
  Using map2Either, we can combine all errors into a single value as
  long as the 'E2' type can act as a container type for multiple error
  values.
  */
  def map2Either[E2 >: E, A2, A3](e: Either[E2, A2])(leftF: (E2, E2) => E2)(rightF: (A, A2) => A3): Either[E2, A3] =
    (this, e) match {
      case (Right(a1), Right(a2)) => Right(rightF(a1, a2))
      case (Left(e), Right(a)) => Left(e)
      case (Right(a), Left(e)) => Left(e)
      case (Left(e1), Left(e2)) => Left(leftF(e1, e2))
    }
}

case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(List.empty): Either[E, List[B]]) { (a, ebs) =>
      f(a).map2(ebs) { (b, bs) => b :: bs }
    }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }
}

