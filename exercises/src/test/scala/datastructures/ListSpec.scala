import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import fpinscala.datastructures._

class ListSpec extends Specification with ScalaCheck {
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

  "drop" should {
    "succeed if drop count less than list length" in {
      List.drop(List(1), 0) mustEqual List(1)
    }

    "succeed if drop count equal to list length" in {
      List.drop(List(1), 1) mustEqual List()
    }

    "fail if drop count greater than list length" in {
      List.drop(List(), 1) must throwA[NotImplementedError]
    }
  }
}

