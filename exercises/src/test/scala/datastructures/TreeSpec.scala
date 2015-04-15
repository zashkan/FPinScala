import org.specs2.mutable.Specification

import fpinscala.datastructures._

class TreeSpec extends Specification {
  val singleNodeTree = Leaf(1)
  val multiNodeTree =
    Branch(
      Branch(
        Leaf(1),
        Branch(
          Leaf(2),
          Leaf(3))),
      Leaf(4))

  "size" should {
    "succeed with single-node tree" in {
      Tree.size(singleNodeTree) mustEqual 1
    }

    "succeed with multi-node tree" in {
      Tree.size(multiNodeTree) mustEqual 7
    }
  }

  "maximum" should {
    "succeed with a single-node tree" in {
      Tree.maximum(singleNodeTree) mustEqual 1
    }

    "succeed with a multi-node tree" in {
      Tree.maximum(multiNodeTree) mustEqual 4
    }
  }


  "depth" should {
    "succeed with a single-node tree" in {
      Tree.depth(singleNodeTree) mustEqual 0
    }

    "succeed with a multi-node tree" in {
      Tree.depth(multiNodeTree) mustEqual 3
    }
  }
}

