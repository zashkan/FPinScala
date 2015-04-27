import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import fpinscala.state._

class StateSpec extends Specification with ScalaCheck {
  "nonNegativeInt" should {
    "always return a non-negative integer" in {
      prop { seed: Long =>
        val rng = RNG.Simple(seed)
        val randNum = RNG.nonNegativeInt(rng)._1

        randNum must beBetween(0, Int.MaxValue)
      }
    }
  }
}

/*
  val emptyIntStream = Stream.empty[Int]
  val stream1 = Stream(1)
  val stream2 = Stream(1, 2)
  val o1 = Option(1)
  val o2 = Option(2)

  def isEven(i: Int) = i % 2 == 0
  def isOdd(i: Int) = i % 2 != 0
  def add(x: Int, y: Int) = x + y
  def add1Stream(i: Int) = Stream(i + 1)

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

  "flatMap" should {
    "obey left identity law" in {
      prop { n: Int =>
        Stream(n).flatMap(add1Stream).toList mustEqual {
          add1Stream(n).toList
        }
      }
    }

    "obey right identity law" in {
      prop { n: Int =>
        Stream(n).flatMap(Stream(_)).toList mustEqual List(n)
      }
    }

    "obey associativity law" in {
      def add2Stream(i: Int) = Stream(i + 2)

      prop { n: Int =>
        val streamN = Stream(n)

        streamN.flatMap(add1Stream).flatMap(add2Stream).toList mustEqual {
          streamN.flatMap(x => add1Stream(x).flatMap(add2Stream)).toList
        }
      }
    }
  }

  "map" should {
    "succeed with an empty stream" in {
      emptyIntStream.map(isOdd) mustEqual emptyIntStream
    }

    "succeed with a non-empty stream" in {
      stream1.map(isOdd).toList mustEqual List(true)
    }
  }

  "filter" should {
    "succeed with an empty stream" in {
      emptyIntStream.filter(isOdd) mustEqual emptyIntStream
    }

    "filter out non-matching items" in {
      stream1.filter(isEven) mustEqual emptyIntStream
    }

    "retain matching items" in {
      stream1.filter(isOdd).toList mustEqual List(1)
    }
  }

  "append" should {
    "append a non-empty stream to an empty stream" in {
      emptyIntStream.append(stream1).toList mustEqual List(1)
    }

    "append an empty stream to a non-empty stream" in {
      stream1.append(emptyIntStream).toList mustEqual List(1)
    }

    "join two non-empty streams" in {
      stream1.append(stream2).toList mustEqual List(1, 1, 2)
    }
  }

  "constant" should {
    "return an infinite stream of the given value" in {
      val x = 1
      val n = 100

      Stream.constant(x).take(n).toList mustEqual List.fill(n)(x)
    }
  }

  "from" should {
    "generate a stream from the given start" in {
      val x = 1
      val n = 100

      Stream.from(x).take(n).toList mustEqual (x to x + n - 1).toList
    }
  }

  "fibs" should {
    "generate Fibonacci numbers" in {
      Stream.fibs.take(10).toList mustEqual {
        List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
      }
    }
  }

  "fibs_unfold" should {
    "generate Fibonacci numbers" in {
      Stream.fibs_unfold.take(10).toList mustEqual {
        List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
      }
    }
  }

  "from_unfold" should {
    "generate a stream from the given start" in {
      val x = 1
      val n = 100

      Stream.from_unfold(x).take(n).toList mustEqual {
        (x to x + n - 1).toList
      }
    }
  }

  "constant_unfold" should {
    "return an infinite stream of the given value" in {
      val x = 1
      val n = 100

      Stream.constant_unfold(x).take(n).toList mustEqual List.fill(n)(x)
    }
  }

  "ones_unfold" should {
    "return an infinite stream of 1s" in {
      val n = 100
      Stream.ones_unfold.take(n).toList mustEqual List.fill(n)(1)
    }
  }

  "map_unfold" should {
    "succeed with an empty stream" in {
      Stream.map_unfold(emptyIntStream)(isOdd) mustEqual emptyIntStream
    }

    "succeed with a non-empty stream" in {
      Stream.map_unfold(stream2)(isOdd).toList mustEqual {
        List(true, false)
      }
    }
  }

  "take_unfold" should {
    "take nothing from an empty stream" in {
      prop { i: Int =>
        Stream.take_unfold(emptyIntStream, i) mustEqual emptyIntStream
      }
    }

    "take entire stream if asked for more than stream contains" in {
      Stream.take_unfold(stream1, 2).toList mustEqual stream1.toList
    }

    "take part of stream if asked for less than stream contains" in {
      Stream.take_unfold(stream2, 1).toList mustEqual List(1)
    }
  }

  "takeWhile_unfold" should {
    "take nothing from an empty stream" in {
      Stream.takeWhile_unfold(emptyIntStream, isEven) mustEqual {
        emptyIntStream
      }
    }

    "take nothing if no items match predicate" in {
      Stream.takeWhile_unfold(stream1, isEven) mustEqual emptyIntStream
    }

    "take items only as long as they match predicate" in {
      Stream.takeWhile_unfold(stream2, isOdd).toList mustEqual List(1)
    }
  }

  "zipWith" should {
    "evaluate to an empty stream if zipping anything to an empty stream" in {
      Stream.zipWith(emptyIntStream, stream1)(add) mustEqual {
        emptyIntStream
      }
    }

    "evaluate to an empty stream if zipping an empty stream to anything" in {
      Stream.zipWith(stream1, emptyIntStream)(add) mustEqual {
        emptyIntStream
      }
    }

    "evaluate to a stream of the shorter length if zipping streams of different lengths" in {
      Stream.zipWith(stream1, stream2)(add).toList mustEqual List(2)
    }

    "evaluate to a stream of the length of the lengths of input streams of equal length" in {
      Stream.zipWith(stream2, stream2)(add).toList mustEqual List(2, 4)
    }
  }

  "zipAll" should {
    "evaluate to an empty stream if zipping empty streams" in {
      emptyIntStream.zipAll(emptyIntStream) mustEqual emptyIntStream
    }

    "evaluate to a stream of pairs with missing elements if zipping streams of unequal length" in {
      stream2.zipAll(stream1).toList mustEqual {
        List(o1 -> o1, Option(2) -> None)
      }
    }

    "evaluate to a stream of pairs with no missing elements if zipping streams of equal length" in {
      stream2.zipAll(stream2).toList mustEqual {
        List(o1 -> o1, o2 -> o2)
      }
    }
  }

  "startsWith" should {
    "succeed for an empty superstream" in {
      emptyIntStream.startsWith(stream1) mustEqual false
    }

    "succeed for an empty substream" in {
      stream1.startsWith(emptyIntStream) mustEqual true
    }

    "succeed for a smaller superstream" in {
      stream1.startsWith(stream2) mustEqual false
    }

    "succeed for a smaller substream" in {
      stream2.startsWith(stream1) mustEqual true
    }
  }

  "tails" should {
    "succeed for an empty stream" in {
      emptyIntStream.tails mustEqual Stream.empty[Stream[Int]]
    }

    "succeed for a non-empty stream" in {
      stream2.tails.flatMap(identity).toList mustEqual List(1, 2, 2)
    }
  }

  "scanRight" should {
    "succeed for an empty stream" in {
      emptyIntStream.scanRight(0)(add).toList mustEqual List(0)
    }

    "succeed for a non-empty stream" in {
      stream2.scanRight(0)(add).toList mustEqual List(3, 2, 0)
    }
  }
}
*/

