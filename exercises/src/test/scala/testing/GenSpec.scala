import org.specs2.mutable.Specification

import fpinscala.state.RNG
import fpinscala.testing._

/*
Ex. 8.1

Some properties (expressed as functions) to test an implementation of
sum: List[Int] => Int

  1. xs: List[Int] => sum(xs) == sum(xs.reverse)

  2. n: Int => x: Int => sum(List.fill(n)(x)) == n * x

  3. sum(List.empty) == 0

  4. x: Int => sum(List(x)) == x

Ex. 8.2

Properties that specify an implementation of maximum: List[Int] => Int

  1. x: Int => maximum(List(x)) == x

  2. n: Int => x: Int => maximum(List.fill(n)(x)) == x

  3. xs: List[Int] => maximum(xs) == maximum(xs.reverse)
*/

class GenSpec extends Specification {
  val simpleRng = RNG.Simple(1)
  val pPass = new Prop { def check = Right(1) }
  val leftFail = Left("Fail" -> 1)
  val pFail = new Prop { def check = leftFail }

  "Prop#&&" should {
    "result in true if all individual Props result in true" in {
      val p = new Prop { def check = Right(1) }
      (pPass && p).check mustEqual Right(2)
    }

    "result in false if some individual Props result in false" in {
      (pPass && pFail).check mustEqual leftFail
    }
  }

  "Gen.choose" should {
    "return a number within the given range" in {
      val start = 0
      val stopExclusive = 10
      val (num, _) =
        Gen
          .choose(start, stopExclusive)
          .asInstanceOf[GenST[Int]]
          .sample
          .run(simpleRng)

      num must beBetween(start, stopExclusive).excludingEnd
    }
  }
}

