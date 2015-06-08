package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
		case Left(e) => Left(e)
		case Right(v) => Right(f(v))
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
		case Left(e) => Left(e)
		case Right(v) => f(v)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
 		case Left(e) => b
 		case Right(v) => Right(v)		
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
 		for {
 			aa <- this
 			bb <- b
 		} yield f(aa, bb)
 }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

case class Person(name: Name, age: Age)
sealed class Name(v: String) 
sealed class Age(v: Int) 


object Either {
	def mkAge(age: Int): Either[String, Age] =
	if (age < 0) Left("negative age!")
	else Right(new Age(age))

	def mkName(name: String): Either[String, Name] =
	if (name.length == 0) Left("empty name!")
	else Right(new Name(name))

	def mkPerson(name: String, age: Int): Either[String, Person] =
		mkName(name).map2(mkAge(age))(Person(_,_))

	def mkListErr(a: Int, b: Int): 
	//List[String] = {
	Either[List[String], Int] = {
		var e: List[String] = List()
		if (a < 0)  e = List("a is zero!")
		if (b < 0)  e = e :+ "b is zero!" 
		if (a < 0 || b < 0) Left(e)
		else Right(a+b)
		}


	def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
		es.foldRight[Either[E, List[B]]](Right(Nil))((h,t) => f(h).map2(t)(_ :: _))

	def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = traverse(es)(x => x)

	def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
		if (xs.isEmpty) 
			Left("mean of empty list!")
		else 
			Right(xs.sum / xs.length)

	def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
		// try Right(x / y)
		// catch { case e: Exception => Left(e) }
		Try(x/y)

	def Try[A](a: => A): Either[Exception, A] =
		try Right(a)
		catch { case e: Exception => Left(e) }

}