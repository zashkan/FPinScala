import org.specs2.mutable.Specification

import fpinscala.datastructures._

class TreeSpec extends Specification {
  "size" should {
    "succeed with single-node tree" in {
      Tree.size(Leaf(1)) mustEqual 1
    }

    "succeed with multi-node tree" in {
      Tree.size(
        Branch(
          Branch(
            Leaf(1),
            Branch(
              Leaf(2),
              Leaf(3))),
          Leaf(4))) mustEqual 7
    }
  }
}

