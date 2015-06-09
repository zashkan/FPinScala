import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import fpinscala.errorhandling._

class EitherSpec extends Specification {
 
 //========================================================================================
 //#4.6
 val myLeft = Left("error string")
 val myRightInput1 = Right(5)
 val myRightInput2= Right(6)
 val myRightOutput1 = Right(10)
 val myRightOutput3 = Right(30)
 def myDoubler(x: Int): Int = 2*x

 def myPositiveDoubler(x: Int): Either[String, Int] = if (x <= 0) Left("non-positive value!") else Right(2*x)
 def myMultiplier(x: Int, y: Int): Int = x*y

  "map" should {
    "return Left for a Left object" in {
      myLeft.map(myDoubler) mustEqual myLeft
    }
   
    "return Right output of the passed function for a Right object" in {
      myRightInput1.map(myDoubler) mustEqual myRightOutput1
    }
  }

  "flatMap" should {
    "return Left for a Left object" in {
      myLeft.flatMap(myPositiveDoubler) mustEqual myLeft
    }
   
    "return Right output of the passed function which outputs Either" in {
      myRightInput1.flatMap(myPositiveDoubler) mustEqual myRightOutput1
    }
  }

  "orElse" should {
    "return the alternative argument for a Left object" in {
      myLeft.orElse(myRightInput1) mustEqual myRightInput1
    }
   
    "return the caller itslef for a Right object" in {
      myRightInput1.orElse(myRightInput2) mustEqual myRightInput1
    }
  }

  "map2" should {
    "return Left for either or both of the inputs being Left" in {
      myLeft.map2(myRightInput1)(myMultiplier) mustEqual myLeft
      myRightInput1.map2(myLeft)(myMultiplier) mustEqual myLeft
      myLeft.map2(myLeft)(myMultiplier) mustEqual myLeft
    }

    "return the Right value of the function for Right inputs" in {
    	myRightInput1.map2(myRightInput2)(myMultiplier) mustEqual myRightOutput3
  	}
  }

  //========================================================================================
  //#4.7
  val allRightList1 = List(Right(1),Right(2),Right(3))
  val allRightList2 = List()

  val hasLeftList1 = List(Right(1),Left("first left object"),Right(3))
  val hasLeftList2 = List(Left("single left object"))

  "sequence" should {
    "return the original list values when it has no Left item" in {
      Either.sequence(allRightList1) mustEqual Right(List(1,2,3))
      Either.sequence(allRightList2) mustEqual Right(Nil)
    }

  "return Left when the list has one or more Left entries" in {
      Either.sequence(hasLeftList1) mustEqual Left("first left object")
      Either.sequence(hasLeftList2) mustEqual Left("single left object")
    }
  }

  val allValueList1 = List(1,2,3)
  val allValueList2 = List(1,-2,3)

  "traverse" should {
    "return the tranformed list values when the tranformation for all items is successful" in {
      Either.traverse(allValueList1)(myPositiveDoubler) mustEqual Right(List(2,4,6))
    }

    "return Left when the tranformation fails for at least one of the items" in {
      Either.traverse(allValueList2)(myPositiveDoubler) mustEqual Left("non-positive value!")
    }
  }

}







