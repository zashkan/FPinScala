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

  "dropWhile" should {
    "drop list prefix elems matching predicate" in {
      List.dropWhile(List(2, 4, 5, 7), { i: Int => i % 2 == 0 }) mustEqual {
        List(5, 7)
      }
    }

    "drop list prefix elems matching predicate upto first non-matching elem but none after" in {
      List.dropWhile(List(2, 4, 5, 6), { i: Int => i % 2 == 0 }) mustEqual {
        List(5, 6)
      }
    }

    "not drop anything if no elems match predicate" in {
      val l = List(2, 4, 5, 6)

      List.dropWhile(l, { i: Int => i < 2 }) mustEqual l
    }
  }

  "init" should {
    "fail for empty list" in {
      List.init(List()) must throwA[NotImplementedError]
    }

    "succeed for a single-elem list" in {
      List.init(List(1)) mustEqual Nil
    }

    "succeed for multi-elem list" in {
      List.init(List(1, 2, 3, 4, 5)) mustEqual List(1, 2, 3, 4)
    }
  }

  /*
  FoldRight is a homomorphism over lists--it applies a given function to
  the list while preserving the shape of the list.
  */
  "foldRight" should {
    "be a homomorphism over lists" in {
      val l = List(1, 2, 3)
      List.foldRight(l, Nil: List[Int])(Cons(_, _)) mustEqual l
    }
  }

  "length" should {
    "succeed for an empty list" in {
      List.length(List()) mustEqual 0
    }

    "succeed for a single-element list" in {
      List.length(List(1)) mustEqual 1
    }

    "succeed for a multi-elem list" in {
      List.length(List(1, 2, 3, 4, 5)) mustEqual 5
    }
  }

  "foldLeft" should {
    "succeed for an empty list" in {
      val l = Nil: List[Nothing]
      List.foldLeft(l, l) { (b, a) => Cons(a, b) } mustEqual l
    }

    "succeed for a single-element list" in {
      val l = List(1)
      List.foldLeft(l, Nil: List[Int]) { (b, a) => Cons(a, b) } mustEqual l
    }

    "succeed for a multi-elem list" in {
      val l = List(1, 2, 3, 4, 5)
      List.foldLeft(l, Nil: List[Int]) { (b, a) => Cons(a, b) } mustEqual {
        List(5, 4, 3, 2, 1)
      }
    }
  }

  "sum3" should {
    "succeed for an empty list" in {
      List.sum3(Nil) mustEqual 0
    }

    "succeed for a single-element list" in {
      List.sum3(List(1)) mustEqual 1
    }

    "succeed for a multi-elem list" in {
      List.sum3(List(1, 2, 3, 4, 5)) mustEqual {
        scala.List(1, 2, 3, 4, 5).sum
      }
    }
  }

  "product3" should {
    "succeed for an empty list" in {
      List.product3(Nil) mustEqual 1
    }

    "succeed for a single-element list" in {
      List.product3(List(1)) mustEqual 1
    }

    "succeed for a multi-elem list" in {
      List.product3(List(1, 2, 3, 4, 5)) mustEqual {
        scala.List(1, 2, 3, 4, 5).product
      }
    }
  }

  "length3" should {
    "succeed for an empty list" in {
      List.length3(Nil) mustEqual 0
    }

    "succeed for a single-element list" in {
      List.length3(List(1)) mustEqual 1
    }

    "succeed for a multi-elem list" in {
      List.length3(List(1, 2, 3, 4, 5)) mustEqual {
        scala.List(1, 2, 3, 4, 5).length
      }
    }
  }

  "reverse" should {
    "succeed for an empty list" in { List.reverse(Nil) mustEqual Nil }

    "succeed for a single-element list" in {
      val l = List(1)
      List.reverse(l) mustEqual l
    }

    "succeed for a multi-elem list" in {
      List.reverse(List(1, 2, 3, 4, 5)) mustEqual List(5, 4, 3, 2, 1)
    }
  }


  "foldRight_using_foldLeft" should {
    "be a homomorphism over lists" in {
      val l = List(1, 2, 3)
      List.foldRight_using_foldLeft(l, Nil: List[Int])(Cons(_, _)) mustEqual l
    }
  }

  "foldLeft_using_foldRight" should {
    "succeed for an empty list" in {
      val l = Nil: List[Nothing]
      List.foldLeft_using_foldRight(l, l) { (b, a) => Cons(a, b) } mustEqual l
    }

    "succeed for a single-element list" in {
      val l = List(1)
      List.foldLeft_using_foldRight(l, Nil: List[Int]) { (b, a) => Cons(a, b) } mustEqual l
    }

    "succeed for a multi-elem list" in {
      val l = List(1, 2, 3, 4, 5)
      List.foldLeft_using_foldRight(l, Nil: List[Int]) { (b, a) => Cons(a, b) } mustEqual {
        List(5, 4, 3, 2, 1)
      }
    }
  }

  "append_using_foldRight" should {
    "succeed for an empty list" in {
      val a2 = List(1, 2, 3)
      List.append_using_foldRight(Nil, a2) mustEqual a2
    }

    "succeed for a non-empty list" in {
      List.append(List(1, 2, 3), List(4, 5, 6)) mustEqual {
        List(1, 2, 3, 4, 5, 6)
      }
    }
  }

  "concat" should {
    "succeed for an empty list" in {
      List.concat(Nil) mustEqual Nil
    }

    "succeed for a non-empty list" in {
      List.concat(List(List(1), List(2), List(3))) mustEqual {
        List(1, 2, 3)
      }
    }
  }

  "add1" should {
    "succeed for an empty list" in { List.add1(Nil) mustEqual Nil }

    "succeed for a non-empty list" in {
      List.add1(List(1, 2, 3)) mustEqual List(2, 3, 4)
    }
  }

  "toString" should {
    "succeed for an empty list" in { List.toString(Nil) mustEqual Nil }

    "succeed for a non-empty list" in {
      List.toString(List(1.0, 2.0, 3.0)) mustEqual List("1.0", "2.0", "3.0")
    }
  }

  "map" should {
    "succeed for an empty list" in {
      List.map(Nil: List[Int])(_ + 1)  mustEqual Nil
    }

    "succeed for a non-empty list" in {
      List.map(List(1, 2, 3))(_ + 1) mustEqual List(2, 3, 4)
    }
  }

  "filter" should {
    "succeed for a list of integers" in {
      List.filter(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0) mustEqual {
        List(2, 4, 6)
      }
    }
  }
}

