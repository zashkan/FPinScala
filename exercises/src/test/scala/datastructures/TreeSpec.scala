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
  val multiNodeTreeMapped =
    Branch(
      Branch(
        Leaf(2),
        Branch(
          Leaf(3),
          Leaf(4))),
      Leaf(5))

  def add1(i: Int) = i + 1

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

  "map" should {
    "succeed with a single-node tree" in {
      Tree.map(singleNodeTree)(add1) mustEqual Leaf(2)
    }

    "succeed with a multi-node tree" in {
      Tree.map(multiNodeTree)(add1) mustEqual multiNodeTreeMapped
    }
  }

  "size_using_fold" should {
    "succeed with single-node tree" in {
      Tree.size_using_fold(singleNodeTree) mustEqual 1
    }

    "succeed with multi-node tree" in {
      Tree.size_using_fold(multiNodeTree) mustEqual 7
    }
  }

  "maximum_using_fold" should {
    "succeed with a single-node tree" in {
      Tree.maximum_using_fold(singleNodeTree) mustEqual 1
    }

    "succeed with a multi-node tree" in {
      Tree.maximum_using_fold(multiNodeTree) mustEqual 4
    }
  }

  "depth_using_fold" should {
    "succeed with a single-node tree" in {
      Tree.depth_using_fold(singleNodeTree) mustEqual 0
    }

    "succeed with a multi-node tree" in {
      Tree.depth_using_fold(multiNodeTree) mustEqual 3
    }
  }

  "map_using_fold" should {
    "succeed with a single-node tree" in {
      Tree.map_using_fold(singleNodeTree)(add1) mustEqual Leaf(2)
    }

    "succeed with a multi-node tree" in {
      Tree.map_using_fold(multiNodeTree)(add1) mustEqual {
        multiNodeTreeMapped
      }
    }
  }
}

