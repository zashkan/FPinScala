import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import fpinscala.errorhandling._

class EitherSpec extends Specification with ScalaCheck {
  val leftObj = Left(1)
  val rightObj = Right(1)
  val rightObj2 = Right(2)

  def add1(i: Int) = i + 1
  def rightAdd1(i: Int) = Right(add1(i))
  def addXY(x: Int, y: Int) = x + y

  "map" should {
    "map a right value using the given function" in {
      rightObj.map(add1) mustEqual rightObj2
    }

    "map a left value to itself" in {
      leftObj.map(add1) mustEqual leftObj
    }
  }

  "flatMap" should {
    "obey left identity law" in {
      prop { n: Int =>
        Right(n).flatMap(rightAdd1) mustEqual rightAdd1(n)
      }
    }

    "obey right identity law" in {
      prop { n: Int =>
        val rightN = Right(n)
        rightN.flatMap(Right(_)) mustEqual rightN
      }
    }

    "obey associativity law" in {
      def f(i: Int) = Right(i + 1)
      def g(i: Int) = Right(i + 1)

      prop { n: Int =>
        val r = Right(n)

        r.flatMap(f).flatMap(g) mustEqual {
          r.flatMap { x => f(x).flatMap(g) }
        }
      }
    }
  }

  "orElse" should {
    "return the object value if it is a right value" in {
      rightObj.orElse(rightObj2) mustEqual rightObj
    }

    "return the default value if the object is a left value" in {
      leftObj.orElse(rightObj2) mustEqual rightObj2
    }
  }

  "map2" should {
    "return mapped value if both values are right" in {
      rightObj.map2(rightObj2)(addXY) mustEqual Right(3)
    }

    "return first left value if first value is left" in {
      leftObj.map2(rightObj)(addXY) mustEqual leftObj
    }

    "return second left value if second value is left" in {
      rightObj.map2(leftObj)(addXY) mustEqual leftObj
    }

    "return first left value if both values are left" in {
      leftObj.map2(Left(2))(addXY) mustEqual leftObj
    }
  }
}

