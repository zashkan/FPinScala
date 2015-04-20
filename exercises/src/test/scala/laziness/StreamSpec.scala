import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import fpinscala.laziness._

class StreamSpec extends Specification with ScalaCheck {
  "toList" should {
    "convert a stream to a list" in {
      prop { l: List[Int] =>
        val s =
          l.foldRight(Stream.empty[Int]) { (a, b) => Stream.cons(a, b) }

        s.toList mustEqual l
      }
    }
  }
}

