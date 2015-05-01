import org.specs2.mutable.Specification
import org.specs2.specification.AfterAll

import java.util.concurrent.{ Executors, TimeoutException, TimeUnit }

import fpinscala.parallelism._

class ParSpec extends Specification with AfterAll {
  def add(x1: Int, x2: Int) = x1 + x2
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
}

