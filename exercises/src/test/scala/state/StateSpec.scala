import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import fpinscala.state._

class StateSpec extends Specification with ScalaCheck {
  def rngFromSeed(seed: Long) = RNG.Simple(seed)
  def betweenZeroAndOneEx(x: Double) =
    x must beBetween(0.0, 1.0).excludingEnd

  "nonNegativeInt" should {
    "always return a non-negative integer" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)
        val randNum = RNG.nonNegativeInt(rng)._1

        randNum must beBetween(0, Int.MaxValue)
      }
    }
  }

  "double" should {
    "always return a non-negative double less than 1" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)
        val randDbl = RNG.double(rng)._1

        betweenZeroAndOneEx(randDbl)
      }
    }
  }

  "intDouble" should {
    "always return an int and a non-negative double less than 1" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)
        val ((i, d), _) = RNG.intDouble(rng)

        i must beBetween(Int.MinValue, Int.MaxValue) and {
          betweenZeroAndOneEx(d)
        }
      }
    }
  }

  "doubleInt" should {
    "always return a non-negative double less than 1 and an int" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)
        val ((d, i), _) = RNG.doubleInt(rng)

        i must beBetween(Int.MinValue, Int.MaxValue) and {
          betweenZeroAndOneEx(d)
        }
      }
    }
  }

  "double3" should {
    "always return three non-negative doubles less than 1" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)
        val ((d1, d2, d3), _) = RNG.double3(rng)

        betweenZeroAndOneEx(d1) and betweenZeroAndOneEx(d1) and {
          betweenZeroAndOneEx(d3)
        }
      }
    }
  }
}

