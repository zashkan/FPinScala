import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import fpinscala.laziness._

class StreamSpec extends Specification with ScalaCheck {
  val emptyIntStream = Stream.empty[Int]
  val stream1 = Stream.cons(1, emptyIntStream)
  val stream2 = Stream.cons(1, Stream.cons(2, emptyIntStream))

  def isEven(i: Int) = i % 2 == 0
  def isOdd(i: Int) = i % 2 != 0

  "toList" should {
    "convert a stream to a list" in {
      Stream.cons(1, Stream.cons(2, Stream.cons(3, emptyIntStream))).toList mustEqual {
        List(1, 2, 3)
      }
    }
  }

  "take" should {
    "take nothing from an empty stream" in {
      prop { i: Int => emptyIntStream.take(i) mustEqual emptyIntStream }
    }

    "take entire stream if asked for more than stream contains" in {
      stream1.take(2).toList mustEqual stream1.toList
    }

    "take part of stream if asked for less than stream contains" in {
      stream2.take(1).toList mustEqual List(1)
    }
  }

  "drop" should {
    "drop nothing from an empty stream" in {
      prop { i: Int => emptyIntStream.drop(i) mustEqual emptyIntStream }
    }

    "drop entire stream if asked to drop more than stream contains" in {
      stream1.drop(2) mustEqual emptyIntStream
    }

    "drop part of stream if asked to drop less than stream contains" in {
      stream2.drop(1).toList mustEqual List(2)
    }
  }

  "takeWhile" should {
    "take nothing from an empty stream" in {
      emptyIntStream.takeWhile(isEven) mustEqual emptyIntStream
    }

    "take nothing if no items match predicate" in {
      stream1.takeWhile(isEven) mustEqual emptyIntStream
    }

    "take items only as long as they match predicate" in {
      stream2.takeWhile(isOdd).toList mustEqual List(1)
    }
  }

  "forAll" should {
    "succeed for empty stream" in {
      emptyIntStream.forAll(isEven) mustEqual true
    }

    "terminate as soon as it finds non-matching value" in {
      /*
      Because Stream.ones is an infinite stream, the very fact that this
      test finishes executing is proof that that forAll terminates as
      soon as it finds a non-matching value.
      */
      Stream.ones.forAll(isEven) mustEqual false
    }
  }

  "headOption" should {
    "succeed for empty stream" in {
      emptyIntStream.headOption mustEqual None
    }

    "succeed for non-empty stream" in {
      stream1.headOption mustEqual Some(1)
    }
  }
}

