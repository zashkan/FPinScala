import org.specs2.mutable.Specification

import fpinscala.errorhandling._

class OptionSpec extends Specification {
  val someObj = Some(1)
  val someOtherObj = Some(2)
  def add1(i: Int) = i + 1
  def addSome1(i: Int) = Some(add1(i))
  def addXY(x: Int, y: Int) = x + y

  "map" should {
    "map a value using the provided function" in {
      someObj.map(add1) mustEqual someOtherObj
    }

    "map an absent value to an absent value" in {
      None.map(add1) mustEqual None
    }
  }

  "getOrElse" should {
    "get the value out of a Some object" in {
      someObj.getOrElse(2) mustEqual 1
    }

    "get the default value if given a None object" in {
      None.getOrElse(2) mustEqual 2
    }
  }

  "flatMap" should {
    "map and then flatten" in {
      someObj.flatMap(addSome1) mustEqual someOtherObj
    }

    "map an absent value" in {
      None.flatMap(addSome1) mustEqual None
    }

    "map to a function that returns an absent value" in {
      someObj.flatMap(_ => None) mustEqual None
    }
  }

  "orElse" should {
    "return the optional if it contains a value" in {
      someObj.orElse(someOtherObj) mustEqual someObj
    }

    "return the default if the optional is missing a value" in {
      None.orElse(someOtherObj) mustEqual someOtherObj
    }
  }

  "filter" should {
    "preserve the optional value if it meets the predicate condition" in {
      someObj.filter(_ == 1) mustEqual someObj
    }

    "drop the optional value if it fails the predicate condition" in {
      someObj.filter(_ != 1) mustEqual None
    }

    "pass through an absent optional value" in {
      None.filter(_ == 1) mustEqual None
    }
  }

  "variance" should {
    "not exist for an empty list" in {
      Option.variance(Seq()) mustEqual None
    }

    "exist for a non-empty list" in {
      Option.variance(Seq(1)) mustEqual Some(0.0)
    }
  }

  "map2" should {
    "return mapped value if both inputs are present" in {
      Option.map2(someObj, someOtherObj)(addXY) mustEqual Some(3)
    }

    "return absent value if first input is absent" in {
      Option.map2(None, someObj)(addXY) mustEqual None
    }

    "return absent value if second input is absent" in {
      Option.map2(someObj, None)(addXY) mustEqual None
    }
  }

  "sequence" should {
    "return some empty list if given an empty list" in {
      Option.sequence(List.empty) mustEqual Some(List.empty)
    }

    "return absent value if given list with any absent values" in {
      Option.sequence(List(someObj, someOtherObj, None)) mustEqual None
    }

    "return some list of values if given list with no absent values" in {
      Option.sequence(List(someObj, someOtherObj)) mustEqual {
        Some(List(1, 2))
      }
    }
  }

  "sequence_using_traverse" should {
    "return some empty list if given an empty list" in {
      Option.sequence_using_traverse(List.empty) mustEqual {
        Some(List.empty)
      }
    }

    "return absent value if given list with any absent values" in {
      Option.sequence_using_traverse(
        List(someObj, someOtherObj, None)
      ) mustEqual None
    }

    "return some list of values if given list with no absent values" in {
      Option.sequence_using_traverse(
        List(someObj, someOtherObj)
      ) mustEqual Some(List(1, 2))
    }
  }
}
