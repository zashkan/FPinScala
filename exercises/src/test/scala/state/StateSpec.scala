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

 //========================================================================================
 //#6.5
  "double_via_map" should {
    "return a bigger than or equal to zero and less than one double Output along with a new State" in {
      prop { 
        y: Long =>
          val rng1 = new RNG.Simple(y)
          val (d, rng2) = RNG.double_via_map(rng1) 
          d must beBetween(0.0, 1.0).excludingEnd
          rng1 mustNotEqual rng2
           }
    }
  }

 //========================================================================================
 //#6.6
  "map2" should {
    "combine two State Actions and apply f to their Outputs and return it along with a new State - unit" in {
      prop { 
        y: Long =>
          val rng1 = new RNG.Simple(y)
          val (a, rng2) = RNG.map2(RNG.unit(1), RNG.unit(2))((_,_))(rng1)
          a mustEqual (1,2)
           }
    }

    "combine two State Actions and apply f to their Outputs and return it along with a new State - Rand" in {
      prop { 
        y: Long =>
          val rng1 = new RNG.Simple(y)
          val (a, rng2) = RNG.map2(RNG.double, RNG.nonNegativeInt)((_,_))(rng1)
          a._1 must beBetween(0.0, 1.0).excludingEnd
          a._2 must beBetween(Int.MinValue, Int.MaxValue)
           }
    }
  }

 //========================================================================================
 //#6.7
  "sequence" should {
    "apply each State Action in a List and return an State Action of a List of Outputs" in {
      prop { 
        y: Long =>
          val rng1 = new RNG.Simple(y)
          val (a, rng2) = RNG.sequence(List(RNG.unit(1), RNG.unit(2)))(rng1)
          a mustEqual List(1,2)
           }
    }
  }

  "ints_via_sequence" should {
    "return an empty List of ints Output along with the same State" in {
      prop { 
        y: Long =>
          val rng1 = new RNG.Simple(y)
          val (as, rng2) = RNG.ints_via_sequence(0)(rng1)         
          as.length mustEqual 0
          rng1 mustEqual rng2
           }
    }

    "return a List of ints Output along with a new State" in {
      prop { 
        y: Long =>
          val rng1 = new RNG.Simple(y)
          val (as, rng2) = RNG.ints_via_sequence(5)(rng1)         
          as.forall(i => (i <= Int.MaxValue) && (i >= Int.MinValue)) mustEqual true
          as.length mustEqual 5
          rng1 mustNotEqual rng2
           }
    }
  }
}