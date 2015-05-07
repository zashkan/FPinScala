import org.specs2.mutable.Specification
import org.specs2.specification.AfterAll

import java.util.concurrent.Executors

import fpinscala.parallelism.Nonblocking._

class NonblockingSpec extends Specification with AfterAll {
  val es = Executors.newCachedThreadPool
  def afterAll = es.shutdown

  "Nonblocking" should {
    "Expose that an exception occurred" in {
      Par.run(es)(Par.delay(1 / 0)) must throwA[ArithmeticException]
    }
  }
}

