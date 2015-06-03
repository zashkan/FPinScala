import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import fpinscala.datastructures._

class ListSpec extends Specification with ScalaCheck {
  //========================================================================================
  //#3.1
  "pattern matching example x" should {
    "result in 3" in {
      List.x mustEqual 3
    }
  }

  //========================================================================================
  //#3.2
  "tail" should {
    "not run on an empty List" in {
      List.tail(List()) must throwA[UnsupportedOperationException]
    }

    "return Nil for single-element List" in {
      prop { 
        x: Int =>
          List.tail(List(x)) mustEqual Nil }
    }

    "return tail of multiple-element List" in {
      List.tail(List(1,2)) mustEqual List(2)
      List.tail(List('a','b')) mustEqual List('b')
    }
  }

  //========================================================================================
  //#3.3
  "setHead" should {
    "update nothing for an empty List" in {
      List.setHead(List(), 0) mustEqual Nil
    }

    "update the head for single-element List" in {
      List.setHead(List(1), 0) mustEqual List(0)
      List.setHead(List('a'), 'z') mustEqual List('z')
    }

    "update head for multiple-element List" in {
      List.setHead(List(1,2), 0) mustEqual List(0,2)
      List.setHead(List('a','b'), 'z') mustEqual List('z','b')
    }
  }

  //========================================================================================
  //#3.4
  "drop" should {
    "result in Nil for empty List" in {
      List.drop(List(), 0) mustEqual Nil
      List.drop(List(), -2) mustEqual Nil
    }

    "result in Nil for single-element List and n=1" in {
      List.drop(List(1), 1) mustEqual Nil
      List.drop(List('a'), 1) mustEqual Nil
    }

    "remove all elements from any List where length=n" in {
      List.drop(List(1,2), 2) mustEqual Nil
      List.drop(List('a','b'), 2) mustEqual Nil
    }

    "remove first n elements for multiple-element List" in {
      List.drop(List(1,2), 1) mustEqual List(2)
      List.drop(List('a','b','c'), 2) mustEqual List('c')
    }

    "not run on a List with length < n" in {
      List.drop(List(1,2), 3) must throwA[UnsupportedOperationException]
      List.drop(List('a','b','c'), 4) must throwA[UnsupportedOperationException]
    }
  }

  //========================================================================================
  //#3.5
  "dropWhile" should {
    "result in Nil for an empty List" in {
      List.dropWhile(List())(x => true) mustEqual Nil
      List.dropWhile(List[Int]())(x => x>5) mustEqual Nil
    }

    "remove the only element in a single-element List matching the predicate" in {
      List.dropWhile(List(1))(_==1) mustEqual Nil
      List.dropWhile(List('a'))(_<'z') mustEqual Nil
    }

    "remove all elements in a multiple-element List matching the predicate" in {
      List.dropWhile(List(1,2,3))(_ < 4) mustEqual Nil
      List.dropWhile(List('a','b','c'))(_<'z') mustEqual Nil
    }

    "remove only first elements matching the predicate in a multiple-element List" in {
      List.dropWhile(List(5,6,1,2,3))(_ > 4) mustEqual List(1,2,3)
      List.dropWhile(List('a','b','c'))(_=='a') mustEqual List('b','c')
    }

    "not remove anything in a List with first element not matching the predicate" in {
      List.dropWhile(List(5,6,1,2,3))(_ < 4) mustEqual List(5,6,1,2,3)
      List.dropWhile(List('a','b','c'))(_=='b') mustEqual List('a','b','c')
    }
  }

  //========================================================================================
  //#3.6
  "init" should {
    "result in Nil for an empty List" in {
      List.init(List()) mustEqual Nil
    }

    "remove the only element in a single-element List" in {
      List.init(List(1)) mustEqual Nil
      List.init(List('a')) mustEqual Nil
    }

    "remove the last element in a multiple-element List" in {
      List.init(List(1,2,3)) mustEqual List(1,2)
      List.init(List('a','b','c')) mustEqual List('a','b')
    }
    //In order to finish the init call the case Cons(_,Nil) => Nil should match. Which means that we have to call init n-1 times where n is the list length
  }

  //========================================================================================
  //#3.8
  "foldRight" should {
    "result in original list when passed the case class constructors" in {
      List.foldRight(List(), Nil:List[Int])(Cons.apply) mustEqual List()
      List.foldRight(List(1,2,3), Nil:List[Int])(Cons.apply) mustEqual List(1,2,3)
    }
  }

  //========================================================================================
  //#3.9
  "length" should {
    "result in 0 for an empty List" in {
      List.length(List()) mustEqual 0
    }

    "result in 1 for a single-element List" in {
      prop { 
        x: Int =>
          List.length(List(x)) mustEqual 1 
        }
    }

    "reslut in the number of elements in a multiple-element List" in {
      List.length(List(1,2,3)) mustEqual 3
      List.length(List('a','b','c')) mustEqual 3
    }
  }

  // //#3.10
  // "foldLeft" should {
  //   "result in original list when passed the case class constructors" in {
  //     List.foldLeft(List(), Nil:List[Int])((b,a) =>Cons(a,b)) mustEqual List()
  //     List.foldLeft(List(1,2,3), Nil:List[Int])((b,a) =>Cons(a,b)) mustEqual List(3,2,1)
  //   }
  // }

 //========================================================================================
 //#3.11
  "sum3 (uses foldLeft)" should {
    "result in zero for an empty List" in {
      List.sum3(List()) mustEqual 0
    }

    "return the element value in a single-element List" in {
      prop { 
        x: Int =>
          List.sum(List(x)) mustEqual x
         }
    }

    "return sum of all elements in a multiple-element List" in {
      List.sum3(List(1,2,3)) mustEqual 6
    }
  }

  "product3 (uses foldLeft)" should {
    "result in one for an empty List" in {
      List.product3(List()) mustEqual 1
    }

    "return the element value in a single-element List" in {
      List.product3(List(3)) mustEqual 3
    }

    "return product of all elements in a multiple-element List" in {
      List.product3(List(1,2,3)) mustEqual 6
      List.product3(List(0,1)) mustEqual 0
    }
  }

  "length2 (uses foldLeft)" should {
    "result in 0 for an empty List" in {
      List.length2(List()) mustEqual 0 
    }

    "result in 1 for a single-element List" in {
      prop { 
        x: Int =>
          List.length2(List(x)) mustEqual 1 
        }
      prop {
        c: Char =>
          List.length2(List(c)) mustEqual 1 
      }
    }

    "reslut in the number of elements in a multiple-element List" in {
      List.length2(List(1,2,3)) mustEqual 3
    }
  }

  //========================================================================================
  //#3.12
  "reverse" should {
    "result in Nil for an empty List" in {
      List.reverse(List()) mustEqual Nil
    }

    "return the same list for a single-element List" in {
      prop { 
        x: Int =>
          List.reverse(List(x)) mustEqual List(x)
        }
      prop {
        c: Char =>
          List.reverse(List(c)) mustEqual List(c) 
      }
    }

    "reslut in a reversed List for a multiple-element List" in {
      List.reverse(List(1,2,3)) mustEqual List(3,2,1)
      List.reverse(List('a','b','c')) mustEqual List('c','b','a')
    }
  }

  //========================================================================================
  //#3.14
  "append" should {
    "result in Nil for two empty Lists" in {
      List.append2(List(), List()) mustEqual Nil
    }

    "return the other list when another List is Nil" in {
      List.append2(List(1),List()) mustEqual List(1)
      List.append2(List(),List('a')) mustEqual List('a')
    }

    "reslut in a List with multiple-elements with first list on the left and second on the right" in {
      List.append2(List(1,2,3),List(4,5)) mustEqual List(1,2,3,4,5)
      List.append2(List('a','b','c'),List('d')) mustEqual List('a','b','c','d')
    }
  }

  //========================================================================================
  //#3.15
  "concatenate" should {
    "result in Nil for two empty Lists" in {
      List.concatenate(List(), List()) mustEqual Nil
    }

    "return the other list when another List is Nil" in {
      List.concatenate(List(1),List()) mustEqual List(1)
      List.concatenate(List(),List('a')) mustEqual List('a')
    }

    "reslut in a List with multiple-elements with first list on the left and second on the right - two lists" in {
      List.concatenate(List(1,2,3),List(4,5)) mustEqual List(1,2,3,4,5)
      List.concatenate(List('a','b','c'),List('d')) mustEqual List('a','b','c','d')
    }

    "reslut in a List with multiple-elements with first list on the left and second in the middle and third on the right - three lists" in {
      List.concatenate(List(1,2,3),List(4,5),List(6)) mustEqual List(1,2,3,4,5,6)
      List.concatenate(List('a','b','c'),List('d','e'),List('f')) mustEqual List('a','b','c','d','e','f')
    }
  }

  //========================================================================================
  //#3.16
  "add1" should {
    "result in Nil for an empty List" in {
      List.add1(List()) mustEqual Nil
    }

    "return a List with 1 added to a single-element List" in {
      List.add1(List(1)) mustEqual List(2)
    }

    "return a List with 1 added to all elements in a multiple-element List" in {
      List.add1(List(1,2,3)) mustEqual List(2,3,4)
    }
  }

  //========================================================================================
  //#3.17
  "listDblToListStr" should {
    "result in Nil for an empty List" in {
      List.listDblToListStr(List()) mustEqual Nil
    }

    "return a List of String for a single-element List of Double" in {
      List.listDblToListStr(List(1.0)) mustEqual Cons("1.0",Nil)
    }

    "return a List of String for a multiple-element List of Double" in {
      List.listDblToListStr(List(1.1,2.2)) mustEqual Cons("1.1", Cons("2.2",Nil))
    }
  }

  //========================================================================================
  //#3.18
  "map" should {
    "result in Nil for an empty List" in {
      List.map(List():List[Int])(_+1) mustEqual Nil
    }

    "apply the fucntion to the single-element List" in {
      List.map(List(1))(_+1) mustEqual List(2)
    }

    "apply the function to all elements in multiple-element List" in {
      List.map(List(1,2))(_*2) mustEqual List(2,4)
    }
  }

  //========================================================================================
  //#3.19
  "filter" should {
    "result in Nil for an empty List" in {
      List.filter(List[Int]())(_>0) mustEqual Nil
    }

    "result in Nill for a List whose elements don't pass the filter" in {
      List.filter(List(1))(_>1) mustEqual Nil
      List.filter(List(0,1))(_>1) mustEqual Nil      
    }

    "remove only elements that pass the filter" in {
      List.filter(List(1,2,3,4,5,6))(_%2==0) mustEqual List(2,4,6)
    }
  }

  //========================================================================================
  //#3.20
  "flatMap" should {
    "result in Nil for an empty List" in {
      List.flatMap(List():List[Int])(x=>List(x,x)) mustEqual Nil
    }

    "apply the map to single-element List" in {
      List.flatMap(List(1))(x => List(x*2)) mustEqual List(2)
    }

    "apply the map to all elements in a multiple-element List" in {
      List.flatMap(List(1,2,3))(x => List(x,x)) mustEqual List(1,1,2,2,3,3)
    }
  }

  //========================================================================================
  //#3.21
  "filter2 (using flagMap)" should {
    "result in Nil for an empty List" in {
      List.filter2(List():List[Int])(_>0) mustEqual Nil
    }

    "result in Nill for a List whose elements don't pass the filter" in {
      List.filter2(List(1))(_>1) mustEqual Nil
      List.filter2(List(0,1))(_>1) mustEqual Nil      
    }

    "remove only elements that pass the filter" in {
      List.filter2(List(1,2,3,4,5,6))(_%2==0) mustEqual List(2,4,6)
    }
  }

  //========================================================================================
  //#3.22
  "addL" should {
    "result in Nil for two empty Lists" in {
      List.addL(List():List[Int], List():List[Int]) mustEqual Nil
    }

    "result in a List with addition of matching elements when the Lists are not the same length" in {
      List.addL(List(1,2), List(4)) mustEqual List(5)
    }

    "result in a List with elementwise addition" in {
      List.addL(List(1,2,3), List(4,3,2)) mustEqual List(5,5,5)
    }
  }

  //========================================================================================
  //#3.23
  "zipWith" should {
    "result in Nil for two empty Lists" in {
      List.zipWith(List():List[Int], List():List[Int])(_+_) mustEqual Nil
    }

    "result in a List with f applied on matching elements only when the Lists are not the same length" in {
      List.zipWith(List(1,2), List(4))(_+_) mustEqual List(5)
    }

    "remove only elements that pass the filter" in {
      List.zipWith(List(1,2,3), List(4,3,2))(_*_) mustEqual List(4,6,6)
    }
  }

  //========================================================================================
  //#3.24
  "hasSubsequence" should {
    "result in true when the sub is empty" in {
      List.hasSubsequence(List():List[Int], List():List[Int]) mustEqual true
      List.hasSubsequence(List(1,2), List():List[Int]) mustEqual true  
    }

    "result in false when sub is not part of sup" in {
      List.hasSubsequence(List(1,2), List(4)) mustEqual false
      List.hasSubsequence(List(1,2), List(1,2,3)) mustEqual false
    }

    "result in true when sub is part of sup" in {
      List.hasSubsequence(List(1,2,3), List(1,2,3)) mustEqual true
      List.hasSubsequence(List(1,2,3), List(2,3)) mustEqual true
      List.hasSubsequence(List(1,2,3), List(1)) mustEqual true
    }
  }

}

