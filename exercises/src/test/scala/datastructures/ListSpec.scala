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

  "setHead" should {
    "replace an empty list with a single-element list" in {
      List.setHead(List(), 1) mustEqual List(1)
    }

    "replace a single-element list with a new single-elem list" in {
      List.setHead(List(1), 2) mustEqual List(2)
    }

    "replace a multi-element list with a new multi-elem list" in {
      List.setHead(List(1, 2), 2) mustEqual List(2, 2)
    }
  }
}

