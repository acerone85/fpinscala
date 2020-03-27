package fpinscala.state

import fpinscala.state.RNG.Simple
import org.joda.time.DateTime
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.prop.{Generator, GeneratorDrivenPropertyChecks}
import org.scalacheck.Prop.forAll


class StateTest extends AnyWordSpec with Matchers with GeneratorDrivenPropertyChecks {
  val implementations: Seq[RNG.Implementation] = Seq(RNG.WithoutMap, RNG.WithHigherOrderFunctions)

  val RNGGenerator: Generator[RNG] = Generator.intGenerator.map(seed => Simple(seed))
  val RNGGen: Gen[RNG] = Gen.choose(Int.MinValue, Int.MaxValue).map(seed => Simple(seed))

  implementations.foreach { implementation =>
    s"RNG::nonNegativeInt (using $implementation)" should {
      "produce only positive integers" in {
        forAll(RNGGenerator) { rng: RNG =>
          RNG.nonNegativeInt(implementation)(rng)._1 should be >= (0)
        }
      }
    }

    s"RNG::double (using $implementation)" should {
      "produce only doubles in the range [0,1]" in {
        forAll(RNGGenerator) { rng: RNG =>
          RNG.double(implementation)(rng)._1 should be (0.5 +- 0.5)
        }
      }
    }


  }
}
