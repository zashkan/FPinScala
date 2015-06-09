import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import fpinscala.errorhandling._

class OptionSpec extends Specification {
  val myNoneOption = None
  val mySomeInput1 = Some(5)
  val mySomeInput2 = Some(6)
  val mySomeOutput1 = Some(10)
  val mySomeOutput2 = Some(12)
  val mySomeOutput3 = Some(30)

  def myDoubler(x: Int): Int = 2*x
  def myPositiveDoubler(x: Int): Option[Int] = if (x <= 0) None else Some(2*x)
  def myMultiplier(x: Int, y: Int): Int = x*y

 //========================================================================================
  //#4.1
  "map" should {
    "return None for a None object" in {
      myNoneOption.map(myDoubler) mustEqual None
    }
   
    "return Some output of the passed function for a Some object" in {
      mySomeInput1.map(myDoubler) mustEqual mySomeOutput1
      mySomeInput2.map(myDoubler) mustEqual mySomeOutput2
    }
  }

  "getOrElse" should {
    "return the default argument for a None object" in {
      myNoneOption.getOrElse(mySomeOutput1) mustEqual mySomeOutput1
    }
   
    "return the value inside caller Some object" in {
      mySomeInput1.getOrElse(mySomeInput2) mustEqual 5
      mySomeInput2.getOrElse(mySomeInput1) mustEqual 6
      mySomeInput2.getOrElse(None) mustEqual 6
    }
  }

  "flatMap" should {
    "return None for a None object" in {
      myNoneOption.flatMap(myPositiveDoubler) mustEqual None
    }
   
    "return output of the passed function which is an Option for a Some object" in {
      mySomeInput1.flatMap(myPositiveDoubler) mustEqual mySomeOutput1
      mySomeInput2.flatMap(myPositiveDoubler) mustEqual mySomeOutput2
    }
  }

  "orElse" should {
    "return the alternative argument for a None object" in {
      myNoneOption.orElse(mySomeInput1) mustEqual mySomeInput1
    }
   
    "return the caller itslef for a Some object" in {
      mySomeInput1.orElse(mySomeInput2) mustEqual mySomeInput1
      mySomeInput2.orElse(mySomeInput1) mustEqual mySomeInput2
    }
  }

  "filter" should {
    "return None for a None object" in {
      myNoneOption.filter(_ == mySomeInput1.getOrElse(0)) mustEqual None
    }
   
    "return the caller itslef for a Some object that passes the filter" in {
       mySomeInput1.filter(_ == mySomeInput1.getOrElse(0)) mustEqual mySomeInput1
    }

    "return None for a Some object that doesn't pass the filter" in {
       mySomeInput1.filter(_ == mySomeInput2.getOrElse(0)) mustEqual None
    }
  }

  //========================================================================================
  //#4.2
  "Vanriance" should {
  	"return None for an empty sequence" in {
      Option.variance(Seq()) mustEqual None
    }
    "return the Some variance value for non-empty sequence" in {
      Option.variance(Seq(1,1)) mustEqual Some(0.0)
    }
  }

  //========================================================================================
  //#4.3
  "map2" should {
    "return None for either or both of the inputs being None" in {
      Option.map2(myNoneOption, mySomeInput1)(myMultiplier) mustEqual None
      Option.map2(mySomeInput1, myNoneOption)(myMultiplier) mustEqual None
      Option.map2(myNoneOption, myNoneOption)(myMultiplier) mustEqual None
    }

    "return the Some value of the function for Some inputs" in {
      Option.map2(mySomeInput1, mySomeInput2)(myMultiplier) mustEqual mySomeOutput3
    }
  }


  //========================================================================================
  //#4.4
  val allSomeList = List(Some(1),Some(2),Some(3))
  val allSomeList2 = List()

  val hasNoneList = List(Some(1),None,Some(2))
  val hasNoneList2 = List(None)

  "sequence" should {
    "return the original list values when it has no None item" in {
      Option.sequence(allSomeList) mustEqual Some(List(1,2,3))
      Option.sequence(allSomeList2) mustEqual Some(Nil)
    }

    "return None when the list has one or more None entries" in {
      Option.sequence(hasNoneList) mustEqual None
      Option.sequence(hasNoneList2) mustEqual None
    }
  }

  //========================================================================================
  //#4.5
  val allValueList = List(1,2,3)
  val allValueList2 = List(1,-2,3)

  "traverse" should {
    "return the tranformed list values when it the tranformation for all items is successful" in {
      Option.traverse(allValueList)(myPositiveDoubler) mustEqual Some(List(2,4,6))
    }

    "return None when the tranformation fails for at least one of the items" in {
      Option.traverse(allValueList2)(myPositiveDoubler) mustEqual None
    }
  }

  "sequence_with_traverse" should {
    "return the original list values when it has no None item" in {
      Option.sequence_with_traverse(allSomeList) mustEqual Some(List(1,2,3))
      Option.sequence_with_traverse(allSomeList2) mustEqual Some(Nil)
    }
  }

}