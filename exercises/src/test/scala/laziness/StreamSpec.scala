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

 //========================================================================================
 //#5.4
  "forAll" should {
    "return true for an empty Stream" in {
      streamEmpty.forAll(_ == 1) mustEqual true
    }

    "return true when all entries satisfy predicate" in {
      streamOfInts.forAll(_ < 4) mustEqual true
    }

    "return false when some entry does not satisfy predicate" in {
      streamOfInts.forAll(_ < 3) mustEqual false
      streamOfInts.forAll(_ < 2) mustEqual false
    }
  }

 //========================================================================================
 //#5.5
  "takeWhile_via_foldRight" should {
    "return an empty Stream for an empty Stream" in {
      streamEmpty.takeWhile_via_foldRight(_ == 1) mustEqual Empty
    }

    "return an empty Stream when the first entry does not satisfy predicate" in {
      streamOfInts.takeWhile_via_foldRight(_ == 2) mustEqual Empty
    }

    "return a Stream with n first entires that satisfy predicate" in {
      streamOfInts.takeWhile_via_foldRight(_ == 1).toList mustEqual Stream(1).toList
    }
  }

 //========================================================================================
 //#5.6
  "headOption_via_foldRight" should {
    "return an None for an empty Stream" in {
      streamEmpty.headOption_via_foldRight mustEqual None
    }

    "return head entry for a none-empty Stream" in {
      streamOfInts.headOption_via_foldRight mustEqual Some(1)
    }
  }

 //========================================================================================
 //#5.7
  "map" should {
    "return an empty Stream for an empty Stream" in {
      streamEmpty.map(identity) mustEqual Empty
    }

    "return a Stream with all entries transformed by f" in {
      streamOfInts.map(_ * 2).toList mustEqual List(2,4,6)
    }
  }

  "filter" should {
    "return an empty Stream for an empty Stream" in {
      streamEmpty.filter(_ == 2) mustEqual Empty
    }

    "return a Stream with only entries that satisfy the filter" in {
      streamOfInts.filter(_%2 == 1).toList mustEqual List(1,3)
    }
  }

  "append" should {
    "return an empty Stream for two empty Streams" in {
      streamEmpty.append(streamEmpty) mustEqual Empty
    }

    "return a Stream with the other Stream is empty" in {
      streamOfInts.append(streamEmpty).toList mustEqual List(1,2,3)
      streamEmpty.append(streamOfInts).toList mustEqual List(1,2,3)
    }

    "return a Stream with both none-empty Streams" in {
      streamOfInts.append(streamOfInts).toList mustEqual List(1,2,3,1,2,3)
    }
  }

  "flatMap" should {
    "return an empty Stream an empty Stream" in {
      streamEmpty.flatMap(x => Stream(x,x)) mustEqual Empty
    }

    "return a Stream with each entry mapped to a new Stream for a none-empty Stream" in {
      streamOfInts.flatMap(x => Stream(x,x)).toList mustEqual List(1,1,2,2,3,3)
    }
  }  

}



























