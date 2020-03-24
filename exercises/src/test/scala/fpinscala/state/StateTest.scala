package fpinscala.state

import fpinscala.state.RNG.Simple
import org.joda.time.DateTime
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalacheck.Prop._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class StateTest extends AnyWordSpec with GeneratorDrivenPropertyChecks with Matchers {
  val implementations: Seq[RNG.Implementation] = Seq(RNG.WithoutMap, RNG.WithHigherOrderFunctions)

  implementations.foreach { implementation =>
    s"RNG::nonNegativeInt (using $implementation)" should {
      "produce only positive integers" in {
        forAll { seed: Int =>
          RNG.nonNegativeInt(implementation)(Simple(seed))._1 should be >= (0)
        }
      }
    }

    s"RNG::double (using $implementation)" should {
      "produce only doubles in the range [0,1]" in {
        forAll { seed: Int =>
          RNG.double(implementation)(Simple(seed))._1 should be (0.5 +- 0.5)
        }
      }
    }


  }
}
