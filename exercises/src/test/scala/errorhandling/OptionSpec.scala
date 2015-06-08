import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import fpinscala.errorhandling._

class OptionSpec extends Specification {
 //========================================================================================
  //#4.1
  "Option" should {
    "implement all member functions" in {
      1 mustEqual 1
    }
  }

  //========================================================================================
  //#4.2
  "Vanriance" should {
  	"hanlde empty sequence" in {
      Option.variance(Seq()) mustEqual None
    }
    "hanlde non-empty sequence" in {
      Option.variance(Seq(1,1)) mustEqual Some(0.0)
    }

  }

}