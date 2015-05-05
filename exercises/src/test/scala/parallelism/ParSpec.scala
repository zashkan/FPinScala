import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.specs2.specification.AfterAll

import java.util.concurrent.{ Executors, TimeoutException, TimeUnit }

import fpinscala.parallelism._

class ParSpec extends Specification with ScalaCheck with AfterAll {
  def add(x1: Int, x2: Int) = x1 + x2
  def add1(x: Int): Int = x + 1
  def isEven(x: Int) = x % 2 == 0
  def max2(x1: Int, x2: Int) = x1.max(x2)

  val nonEmptyList = (1 to 5).toList
  val emptyList = List.empty[Int]

  val es = Executors.newCachedThreadPool

  def afterAll = es.shutdown

  "Par.map2" should {
    "not timeout if the individual futures don't timeout" in {
      val fut = Par.run(es)(Par.map2(Par.unit(1), Par.unit(2))(add))

      fut.get(Long.MaxValue, TimeUnit.MILLISECONDS) mustEqual 3
    }

    /*
    We can't test that map2 returns a Future that times out because the
    individual Futures are UnitFutures which will actually never time
    out.
    */
  }

  "Par.asyncF" should {
    "succeed at making a function asynchronous" in {
      val par = Par.asyncF(add1)(1)
      val fut = Par.run(es)(par)

      fut.get mustEqual 2
    }
  }

  "Par.sequence" should {
    "succeed with an empty list" in {
      val fut = Par.run(es)(Par.sequence(List.empty[Par.Par[Int]]))
      fut.get mustEqual List.empty[Int]
    }

    "succeed with a non-empty list" in {
      val fut = Par.run(es)(Par.sequence(nonEmptyList.map(Par.unit(_))))
      fut.get mustEqual nonEmptyList
    }
  }

  "Par.parFilter" should {
    "succeed with an empty list" in {
      val fut = Par.run(es)(Par.parFilter(emptyList)(isEven))
      fut.get mustEqual emptyList
    }

    "succeed with a non-empty list" in {
      val fut = Par.run(es)(Par.parFilter(nonEmptyList)(isEven))
      fut.get mustEqual nonEmptyList.filter(isEven)
    }
  }

  "Par.parFold" should {
    "succeed with an arbitrary IndexedSeq" in {
      prop { xs: IndexedSeq[Int] =>
        val zMin = Int.MinValue
        val seqMax = xs.fold(zMin)(max2)
        val fut = Par.run(es)(Par.parFold(xs, zMin)(identity)(max2))

        fut.get mustEqual seqMax
      }
    }
  }

  "Par.wordCount" should {
    "succeed with arbitrary paragraphs" in {
      prop { paras: List[String] =>
        val expectedCount =
          paras.foldRight(0) { (a, b) => b + a.split(' ').length }
        val fut = Par.run(es)(Par.wordCount(paras))

        fut.get mustEqual expectedCount
      }
    }
  }
}

