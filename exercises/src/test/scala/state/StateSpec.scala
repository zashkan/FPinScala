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

  "ints" should {
    "return an empty list" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)

        RNG.ints(0)(rng)._1 mustEqual List.empty[Int]
      }
    }

    "return a non-empty list" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)
        val length = 10

        RNG.ints(length)(rng)._1.length mustEqual length
      }
    }
  }

  "double_via_map" should {
    "always return a non-negative double less than 1" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)
        val randDbl = RNG.double_via_map(rng)._1

        betweenZeroAndOneEx(randDbl)
      }
    }
  }

  "map2" should {
    "combine two random actions into one" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)

        betweenZeroAndOneEx(
          RNG.map2(RNG.double_via_map, RNG.double_via_map)(_ * _)(rng)._1
        )
      }
    }
  }

  "sequence" should {
    "join an empty list into a random state transition over an empty list" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)
        val l = List.empty[RNG.Rand[Int]]

        RNG.sequence(l)(rng)._1 mustEqual l
      }
    }

    "join a list of random state transitions into a random state transition over a list" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)
        val l = List(1, 2, 3)

        RNG.sequence(l.map(RNG.unit))(rng)._1 mustEqual l
      }
    }
  }
}

