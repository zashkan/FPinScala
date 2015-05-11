import org.specs2.mutable.Specification
import org.specs2.specification.AfterAll

import java.util.concurrent.Executors

import fpinscala.parallelism.Nonblocking._

class NonblockingSpec extends Specification with AfterAll {
  val es = Executors.newCachedThreadPool
  def afterAll = es.shutdown

  val kvMap = Map(1 -> Par.unit("a"), 2 -> Par.unit("b"))
  val p1 = Par.unit(1)
  val p2 = Par.unit(2)

  "Nonblocking" should {
    "Expose that an exception occurred" in {
      Par.run(es)(Par.delay(1 / 0)) must throwA[ArithmeticException]
    }
  }

  "choiceViaChoiceN" should {
    "succeed for true choice" in {
      Par.run(es)(Par.choiceViaChoiceN(Par.unit(true))(Par.unit(1), Par.unit(0))) mustEqual 1
    }

    "succeed for false choice" in {
      Par.run(es)(Par.choiceViaChoiceN(Par.unit(false))(Par.unit(1), Par.unit(0))) mustEqual 0
    }
  }

  "choiceMap" should {
    "succeed if key in map" in {
      Par.run(es)(Par.choiceMap(p1)(kvMap)) mustEqual "a"
    }

    "fail if key not in map" in {
      Par.run(es)(Par.choiceMap(Par.unit(3))(kvMap)) must {
        throwA[NoSuchElementException]
      }
    }
  }

  "choiceViaChooser" should {
    "succeed for true choice" in {
      Par.run(es)(Par.choiceViaChooser(Par.unit(true))(p1, p2)) mustEqual 1
    }

    "succeed for false choice" in {
      Par.run(es)(Par.choiceViaChooser(Par.unit(false))(p1, p2)) mustEqual 2
    }
  }

  "choiceNChooser" should {
    "succeed if choice n in list" in {
      Par.run(es)(Par.choiceNChooser(Par.unit(0))(List(p1, p2))) mustEqual 1
    }

    "fail if choice n not in list" in {
      Par.run(es)(Par.choiceNChooser(p2)(List(p1, p2))) must {
        throwA[IndexOutOfBoundsException]
      }
    }
  }
}

