import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import fpinscala.state._

class StateSpec extends Specification with ScalaCheck {
 
 def beBetween0And1Ex(d: Double) =
  d must beBetween(0.0, 1.0).excludingEnd
 //========================================================================================
 //#6.1
  "nonNegativeInt" should {
    "return a bigger than or equal to zero Int Output along with a new State" in {
      prop { 
        x: Int =>
          val rng1 = RNG.Simple(x)
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
          val rng1 = RNG.Simple(y)
          val (d, rng2) = RNG.double(rng1) 
          //d must beBetween(0.0, 1.0).excludingEnd
          beBetween0And1Ex(d)
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
          val rng1 = RNG.Simple(y)
          val ((i,d), rng2) = RNG.intDouble(rng1) 
          beBetween0And1Ex(d)
          i must beBetween(Int.MinValue, Int.MaxValue)
          rng1 mustNotEqual rng2
           }
    }
  }

  "doubleInt" should {
    "return a pair of Double and Int Output along with a new State" in {
      prop { 
        y: Long =>
          val rng1 = RNG.Simple(y)
          val ((d,i), rng2) = RNG.doubleInt(rng1) 
          beBetween0And1Ex(d)
          i must beBetween(Int.MinValue, Int.MaxValue)
          rng1 mustNotEqual rng2
           }
    }
  }

  "double3" should {
    "return a tuple of three Double Output along with a new State" in {
      prop { 
        y: Long =>
          val rng1 = RNG.Simple(y)
          val ((d1, d2, d3), rng2) = RNG.double3(rng1) 
          beBetween0And1Ex(d1)
          beBetween0And1Ex(d2)
          beBetween0And1Ex(d3)
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
          val rng1 = RNG.Simple(y)
          val (as, rng2) = RNG.ints(0)(rng1)         
          as.length mustEqual 0
          rng1 mustEqual rng2
           }
    }

    "return a List of ints Output along with a new State" in {
      prop { 
        y: Long =>
          val rng1 = RNG.Simple(y)
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
          val rng1 = RNG.Simple(y)
          val (d, rng2) = RNG.double_via_map(rng1) 
          beBetween0And1Ex(d)
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
          val rng1 = RNG.Simple(y)
          val (a, rng2) = RNG.map2(RNG.unit(1), RNG.unit(2))((_,_))(rng1)
          a mustEqual (1,2)
           }
    }

    "combine two State Actions and apply f to their Outputs and return it along with a new State - Rand" in {
      prop { 
        y: Long =>
          val rng1 = RNG.Simple(y)
          val (a, rng2) = RNG.map2(RNG.double, RNG.nonNegativeInt)((_,_))(rng1)
          beBetween0And1Ex(a._1)
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
          val rng1 = RNG.Simple(y)
          val (a, rng2) = RNG.sequence(List(RNG.unit(1), RNG.unit(2)))(rng1)
          a mustEqual List(1,2)
          rng1 mustEqual rng2
           }
    }
  }

  "ints_via_sequence" should {
    "return an empty List of ints Output along with the same State" in {
      prop { 
        y: Long =>
          val intCount = 0
          val rng1 = RNG.Simple(y)
          val (as, rng2) = RNG.ints_via_sequence(intCount)(rng1)         
          as.length mustEqual intCount
          rng1 mustEqual rng2
           }
    }

    "return a List of ints Output along with a new State" in {
      prop { 
        y: Long =>
          val intCount = 5
          val rng1 = RNG.Simple(y)
          val (as, rng2) = RNG.ints_via_sequence(intCount)(rng1)         
          as.forall(i => (i <= Int.MaxValue) && (i >= Int.MinValue)) mustEqual true
          as.length mustEqual intCount
          rng1 mustNotEqual rng2
           }
    }
  }

 //========================================================================================
 //#6.8
  "flatMap" should {
    "apply the first State Action to obtain an Output then applies g on that output to obtain and run a new State Action" in {
      prop { 
        y: Long =>
          val rng1 = RNG.Simple(y)
          val (a, rng2) = RNG.flatMap(RNG.unit(1))(x => RNG.unit(x+2))(rng1)
          a mustEqual 3
           }
    }
  }

  "nonNegativeLessThan" should {
    "return a positive Int which is less than n" in {
      prop { 
        y: Long =>
          val maxVal = 10
          val rng1 = RNG.Simple(y)
          val (a, rng2) = RNG.nonNegativeLessThan(maxVal)(rng1)
          a must beBetween(0, maxVal).excludingEnd 
           }
    }
  }
  
 //========================================================================================
 //#6.9
 "map_via_flatMap" should {
    "apply f to the State Action output" in {
      prop { 
        y: Long =>
          val rng1 = RNG.Simple(y)
          val (a, rng2) = RNG.map_via_flatMap(RNG.unit(5))(_*2)(rng1)
          a mustEqual 10
           }
    }
  }

  "map2_via_flatMap" should {
    "combine two State Actions and apply f to their Outputs and return it along with a new State - unit" in {
      prop { 
        y: Long =>
          val rng1 = RNG.Simple(y)
          val (a, rng2) = RNG.map2_via_flatMap(RNG.unit(1), RNG.unit(2))((_,_))(rng1)
          a mustEqual (1,2)
           }
    }

    "combine two State Actions and apply f to their Outputs and return it along with a new State - Rand" in {
      prop { 
        y: Long =>
          val rng1 = RNG.Simple(y)
          val (a, rng2) = RNG.map2_via_flatMap(RNG.double, RNG.nonNegativeInt)((_,_))(rng1)
          beBetween0And1Ex(a._1)
          a._2 must beBetween(Int.MinValue, Int.MaxValue)
           }
    }
  }

 //========================================================================================
 //#6.10
 "State.unit" should {
    "return a State with a run member that does not change state and outputs the input" in {
      prop {
        y: Long =>
          val rng1 = RNG.Simple(y)
          val (a, rng2) = State.unit(5).run(rng1)
          a mustEqual 5
          rng1 mustEqual rng2
      }
    }
 }

 "State.map" should {
    "apply f to the State Action output - unit" in {
      prop { 
        y: Long =>
          val rng1 = RNG.Simple(y)
          val (a, rng2) = State.unit(5).map(_*2).run(rng1)
          a mustEqual 10
          rng1 mustEqual rng2
           }
    }

    "apply f to the State Action output - Int" in {
      prop { 
        y: Long =>
          val rng1 = RNG.Simple(y)
          val (a, rng2) = State[RNG, Int](_.nextInt).map(x => x - x%2).run(rng1)
          a%2 mustEqual 0
          rng1 mustNotEqual rng2
           }
    }
  }

  "State.map2" should {
    "combine two State Actions and apply f to their Outputs and return it along with a new State - unit" in {
      prop { 
        y: Long =>
          val rng1 = RNG.Simple(y)
          val s: State[RNG, Int] = State.unit(1)
          val (a, rng2) = s.map2(s)((x,y) => (x,y+1)).run(rng1)
          a mustEqual (1,2)
          rng1 mustEqual rng2
           }
    }

    "combine two State Actions and apply f to their Outputs and return it along with a new State - Rand" in {
      prop { 
        y: Long =>
          val rng1 = RNG.Simple(y)
          val (a, rng2) = State(RNG.double).map2(State(RNG.nonNegativeInt))((_,_)).run(rng1)
          beBetween0And1Ex(a._1)
          a._2 must beBetween(Int.MinValue, Int.MaxValue)
          rng1 mustNotEqual rng2
           }
    }
  }

  "State.flatMap" should {
    "apply the first State Action to obtain an Output then applies g on that output to obtain and run a new State Action" in {
      prop { 
        y: Long =>
          val rng1 = RNG.Simple(y)
          val s: State[RNG, Int] = State.unit(1)
          val f = (x: Int) => State.unit(x+2): State[RNG, Int]
          val (a, rng2) = s.flatMap(f).run(rng1)
          a mustEqual 3
          rng1 mustEqual rng2
           }
    }
  }

  "State.sequence" should {
    "apply each State Action in a List and return an State Action of a List of Outputs" in {
      prop { 
        y: Long =>
          val rng1 = RNG.Simple(y)
          val xs = List(1,2)
          //val f: State[RNG, Int] = State.unit
          
          val (ys, rng2) = State.sequence(xs.map(State.unit[RNG, Int])).run(rng1)
          
          //Works
          // val f = (a: Int) => State(a -> _): State[RNG, Int] 
          // val (ys, rng2) = State.sequence(xs.map(f)).run(rng1)

          //substitution of f works-Referential transparency 
          //val f = State.unit[RNG, Int]
          //val (ys, rng2) = State.sequence(xs.map(f)).run(rng1)

          ys mustEqual xs
          rng1 mustEqual rng2
           }
    }
  }

 //========================================================================================
 //#6.10
 //ToDo

}