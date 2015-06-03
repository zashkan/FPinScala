import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import fpinscala.datastructures._

class TreeSpec extends Specification {
  val singleLeafTree = Leaf(0)
  val multipleLeafTree = 
  Branch(
    Branch(Leaf(1),Leaf(2)),
    Branch(
        Branch(Leaf(5),Leaf(90)),
        Leaf(40)))

  //========================================================================================
  //#3.25
  "size" should {
    "return one for singleLeafTree" in {
      Tree.size(singleLeafTree) mustEqual 1
    }

    "return number of nodes for multipleLeafTree" in {
      Tree.size(multipleLeafTree)  mustEqual 9
    }
  }

  //========================================================================================
  //#3.26
  "maximum" should {
    "return the node value for singleLeafTree" in {
      Tree.maximum(singleLeafTree) mustEqual 0
    }

    "return maximum node value for multipleLeafTree" in {
      Tree.maximum(multipleLeafTree)  mustEqual 90
    }
  }

  //========================================================================================
  //#3.27
  "depth" should {
    "return zero for singleLeafTree" in {
      Tree.depth(singleLeafTree) mustEqual 0
    }

    "return longest path to a leaf for multipleLeafTree" in {
      Tree.depth(multipleLeafTree)  mustEqual 3
    }
  }

  //========================================================================================
  //#3.28
  "map" should {
    "apply function to the node in singleLeafTree" in {
      Tree.map(singleLeafTree)(_+5) mustEqual Leaf(5)
    }

    "apply function to every node in multipleLeafTree" in {
      Tree.map(multipleLeafTree)(_*10)  mustEqual 
          Branch(
            Branch(Leaf(10),Leaf(20)),
            Branch(
                Branch(Leaf(50),Leaf(900)),
                Leaf(400)))
    }     
    "apply function to every node in multipleLeafTree" in {
      Tree.map(multipleLeafTree)(x => List(x,x))  mustEqual 
          Branch(
            Branch(Leaf(List(1,1)),Leaf(List(2,2))),
            Branch(
                Branch(Leaf(List(5,5)),Leaf(List(90,90))),
                Leaf(List(40,40))))
    }
  }

 //========================================================================================
  //#3.29
  "size2" should {
    "return one for singleLeafTree" in {
      Tree.size2(singleLeafTree) mustEqual 1
    }

    "return number of nodes for multipleLeafTree" in {
      Tree.size2(multipleLeafTree)  mustEqual 9
    }
  }

  //========================================================================================
  "maximum2" should {
    "return the node value for singleLeafTree" in {
      Tree.maximum2(singleLeafTree) mustEqual 0
    }

    "return maximum node value for multipleLeafTree" in {
      Tree.maximum2(multipleLeafTree)  mustEqual 90
    }
  }

  //========================================================================================
  "depth2" should {
    "return zero for singleLeafTree" in {
      Tree.depth2(singleLeafTree) mustEqual 0
    }

    "return longest path to a leaf for multipleLeafTree" in {
      Tree.depth2(multipleLeafTree)  mustEqual 3
    }
  }

  //========================================================================================
  "map2" should {
    "apply function to the node in singleLeafTree" in {
      Tree.map2(singleLeafTree)(_+5) mustEqual Leaf(5)
    }

    "apply function to every node in multipleLeafTree" in {
      Tree.map2(multipleLeafTree)(_*10)  mustEqual 
          Branch(
            Branch(Leaf(10),Leaf(20)),
            Branch(
                Branch(Leaf(50),Leaf(900)),
                Leaf(400)))
    }     
    "apply function to every node in multipleLeafTree" in {
      Tree.map2(multipleLeafTree)(x => List(x,x))  mustEqual 
          Branch(
            Branch(Leaf(List(1,1)),Leaf(List(2,2))),
            Branch(
                Branch(Leaf(List(5,5)),Leaf(List(90,90))),
                Leaf(List(40,40))))
    }

  }

}