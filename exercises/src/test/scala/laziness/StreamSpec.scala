import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import fpinscala.laziness._

class StreamSpec extends Specification {
 
 //========================================================================================
 //#5.1
 val streamEmpty = Stream()
 val streamOfInts = Stream(1,2,3)

  "toList" should {
    "return an empty List for an empty Stream" in {
      streamEmpty.toList mustEqual List()
    }

    "return a List with entires in the Stream with the same order" in {
      streamOfInts.toList mustEqual List(1,2,3)
    }
  }

 //========================================================================================
 //#5.2
  "take" should {
    "return an empty Stream for an empty Stream" in {
      streamEmpty.take(3) mustEqual Empty
    }

    "return a Stream with n first entires" in {
      streamOfInts.take(1).toList mustEqual Stream(1).toList
    }
  }

  "drop" should {
    "return an empty Stream for an empty Stream" in {
      streamEmpty.drop(3) mustEqual Empty
    }

    "return an empty Stream when n is bigger than or equal to Stream length" in {
      streamOfInts.drop(4) mustEqual Empty
    }

    "return a Stream with n first entires removed" in {
      streamOfInts.drop(1).toList mustEqual Stream(2,3).toList
    }
  }

 //========================================================================================
 //#5.3
  "takeWhile" should {
    "return an empty Stream for an empty Stream" in {
      streamEmpty.takeWhile(_ == 1) mustEqual Empty
    }

    "return an empty Stream when the first entry does not satisfy predicate" in {
      streamOfInts.takeWhile(_ == 2) mustEqual Empty
    }

    "return a Stream with n first entires that satisfy predicate" in {
      streamOfInts.takeWhile(_ == 1).toList mustEqual Stream(1).toList
    }
  }

}