package fpinscala.laziness

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import Stream._


class StreamTest extends AnyWordSpec with Matchers {

  val implementations: Seq[Stream.Implementation] = Seq(FoldRight, NoFoldRight)

  def from(n: Int): Stream[Int] = cons(n, from(n+1))
  lazy val allInts: Stream[Int] = from(0)

  "Stream::toList" should {
    "convert the empty stream to the empty list" in {
      Stream.empty.toList should be (Nil)
    }

    "convert a finite stream into a list with the same values" in {
      Stream(1, 2, 3).toList should be(List(1, 2, 3))
    }
  }

  "Stream::take" should {
    "return the empty value when 0 elements are taken" in {
      allInts.take(0) should be(Stream.empty)
    }

    "return the first n elements of a Stream" in {
      //this test will fail if trying to compare streams directly,
      //as the head and tails of streams are functions
      //and functions will be compared by their reference address
      //therefore streams will be coerced to lists before comparing them
      allInts.take(4).toList should be (0 until 4)
    }
  }

  "Stream::drop" should {

    "return the same stream that invoked the method when 0 elements are dropped" in {
      allInts.drop(0) should be (allInts)
    }

    "return the empty stream if more elements than the size of the streams are dropped" in {
      Stream.empty.drop(5) should be(Stream.empty)
    }

    "drop the first n elements from a Stream" in {
      allInts.take(7).drop(4).toList  should be(4 until 7)
    }
  }

  implementations.foreach { implementation =>
    s"Stream::takeWhile (when using $implementation implementation)" should {

      "return the empty Stream when the always false predicate is used" in {
        allInts.takeWhile( _ => false, implementation) should be (Empty)
      }

      "return the input Stream if it is finite, when the always true predicate is used" in {
        (allInts.take(10).takeWhile( _ => true, implementation).toList
          should be (0 until 10))
      }

      "return the largest prefix of the invoking stream that satisfies the input predicate" in {
        allInts.takeWhile(_ < 10, implementation).toList should be (0 until 10)
      }
    }
  }

}
