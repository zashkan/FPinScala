import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import fpinscala.state._

class StateSpec extends Specification with ScalaCheck {
 
 //========================================================================================
 //#6.1
  "nonNegativeInt" should {
    "return a bigger than or equal to zero Int Output along with a new State" in {
      prop { 
        x: Int =>
          val rng1 = new RNG.Simple(x)
          val (i, rng2) = RNG.nonNegativeInt(rng1) 
          (i >= 0) mustEqual true
          rng2 mustNotEqual rng1
           }
    }
  }

 //========================================================================================
 //#6.2
  "double" should {
    "return a bigger than or equal to zero and less than one double Output along with a new State" in {
      prop { 
        y: Long =>
          val rng1 = new RNG.Simple(y)
          val (d, rng2) = RNG.double(rng1) 
          d must beBetween(0.0, 1.0).excludingEnd
          rng1 mustNotEqual rng2
           }
    }
  }

 //========================================================================================
 //#6.3
  "intDouble" should {
    "return a pair of Int and Double Output along with a new State" in {
      prop { 
        y: Long =>
          val rng1 = new RNG.Simple(y)
          val ((i,d), rng2) = RNG.intDouble(rng1) 
          d must beBetween(0.0, 1.0).excludingEnd
          i must beBetween(Int.MinValue, Int.MaxValue)
          rng1 mustNotEqual rng2
           }
    }
  }

  "doubleInt" should {
    "return a pair of Double and Int Output along with a new State" in {
      prop { 
        y: Long =>
          val rng1 = new RNG.Simple(y)
          val ((d,i), rng2) = RNG.doubleInt(rng1) 
          d must beBetween(0.0, 1.0).excludingEnd
          i must beBetween(Int.MinValue, Int.MaxValue)
          rng1 mustNotEqual rng2
           }
    }
  }

  "double3" should {
    "return a tuple of three Double Output along with a new State" in {
      prop { 
        y: Long =>
          val rng1 = new RNG.Simple(y)
          val ((d1, d2, d3), rng2) = RNG.double3(rng1) 
          d1 must beBetween(0.0, 1.0).excludingEnd
          d2 must beBetween(0.0, 1.0).excludingEnd
          d3 must beBetween(0.0, 1.0).excludingEnd
          rng1 mustNotEqual rng2
           }
    }
  }

 //========================================================================================
 //#6.4
  "ints" should {
    "return an empty List of ints Output along with the same State" in {
      prop { 
        y: Long =>
          val rng1 = new RNG.Simple(y)
          val (as, rng2) = RNG.ints(0)(rng1)         
          as.length mustEqual 0
          rng1 mustEqual rng2
           }
    }

    "return a List of ints Output along with a new State" in {
      prop { 
        y: Long =>
          val rng1 = new RNG.Simple(y)
          val (as, rng2) = RNG.ints(5)(rng1)         
          as.forall(i => (i <= Int.MaxValue) && (i >= Int.MinValue)) mustEqual true
          as.length mustEqual 5
          rng1 mustNotEqual rng2
           }
    }
  }

 // //========================================================================================
 // //#5.8
 //  "constant" should {
 //    "return an infinite Stream with entries equal to the value passed" in {
 //      prop {
 //        x: Int =>
 //          Stream.constant(x).exists(_ == x) mustEqual true
 //          Stream.constant(x).take(2).toList mustEqual List(x,x)
 //      }
 //    }
 //  }  


}