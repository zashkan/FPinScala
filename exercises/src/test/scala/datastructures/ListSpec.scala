import org.specs2.mutable.Specification
import fpinscala.datastructures._

class ListSpec extends Specification {
  "Exercise 3.1" should {
    "result in x being 3" in {
      List.x mustEqual 3
    }
  }

  "tail" should {
    "not be implemented for empty list" in {
      List.tail(List()) must throwA[NotImplementedError]
    }

    "be implemented for single-element list" in {
      List.tail(List(1)) mustEqual Nil
    }

    "be implemented for multi-element list" in {
      List.tail(List(1, 2)) mustEqual List(2)
    }
  }
}

