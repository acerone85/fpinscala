package fpinscala.laziness

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import Stream._


class StreamTest extends AnyWordSpec with Matchers {

  val implementations: Seq[Stream.Implementation] = Seq(FoldRight, NoFoldRight)

  def from(n: Int): Stream[Int] = cons(n, from(n+1))
  def constant(n: Int): Stream[Int] = cons(n, constant(n))
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

    s"Stream::exists (when using $implementation implementation)" should {
      "return false if invoked by the Empty stream" in {
        Empty.exists(_ => true, implementation) should be(false)
      }

      "return false if invoked by a finite stream using the always false predicate as argument" in {
        allInts.take(5).exists(_ => false, implementation) should be(false)
      }

      "return true if invoked by an infinite stream using a predicate" +
        " that is satisfied by one of the stream elements" in {
        allInts.exists(_ == 10, implementation) should be(true)
      }
    }

    s"Stream::forall (when using $implementation implementation)" should {
      "return true if invoked by the Empty stream" in {
        Empty.forall(_ => false, implementation) should be(true)
      }

      "return true if invoked by a finite stream using the always true predicate as argument" in {
        allInts.take(5).forall(_ => true, implementation) should be(true)
      }

      "return false if invoked by an infinite stream using a predicate" +
        " that is not satisfied by one of the stream elements" in {
        allInts.forall(_ != 10, implementation) should be(false)
      }
    }

    s"Stream::headOption (when using $implementation implementation)" should {
      "return None when invoked by the Empty stream" in {
        Empty.headOption(implementation) should be (None)
      }

      "return the first element of an infinite stream" in {
        allInts.headOption(implementation) should be(Some(0))
      }
    }

    s"Stream::map (when using $implementation)" should {
      "map Empty to Empty" in {
        Empty.map(_ => throw new Exception("test failed"), implementation) should be (Empty)
      }

      "map the elements of Stream lazily" in {
        allInts.map(_ + 1, implementation).take(5).toList should be(List(1, 2, 3, 4, 5))
      }
    }

    s"Stream::filter (when using $implementation)" should {
      "return the Empty stream when applied to the Empty stream" in {
        Empty.filter(_ => true) should be(Empty)
      }

      "filter out the elements of a Stream lazily" in {
        allInts.filter(_ % 2 == 0).take(5).toList should be(List(0, 2, 4, 6, 8))
      }
    }

    s"Stream::append (when using $implementation)" should {
      "return the argument Stream when appended to the Empty stream" in {
        Empty.append(allInts, implementation) should be (allInts)
      }

      "correctly append a Stream at the end of a finite one" in {
        (allInts.take(3)
          .append(allInts.drop(3).take(3), implementation)
          .toList
          should be(List(0, 1, 2, 3, 4, 5)))
      }
    }

    s"Stream::flatMap (when using $implementation)" should {
      "return the Empty stream when applied to the Empty stream" in {
        Empty.flatMap(_ => allInts, implementation) should be (Empty)
      }

      "behave as the identity when applied to the function n => Stream(n)" in {
        allInts.flatMap(n => Stream(n)).take(5).toList should be (List(0, 1, 2, 3, 4))
      }

      "ignore the tail of a stream if f(head) returns an infinite stream" in {
        allInts.flatMap( n => constant(n)).take(5).toList should be (List(0, 0, 0, 0, 0))
      }
    }
  }

  "Stream.fibs" should {
    "generate correctly the first ten fibonacci numbers" in {
      Stream.fibs.take(10).toList should be (List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
    }
  }

  "Stream.fibsUnfold" should {
    "generate correctly the first ten fibonacci numbers" in {
      Stream.fibsUnfold.take(10).toList should be (List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
    }
  }

  "Stream.fromUnfold" should {
    "generate correctly the first ten integers" in {
      Stream.fromUnfold(0).take(10).toList should be (0 until 10)
    }
  }

  "Stream.WithUnfold::take" should {
    "return the empty value when 0 elements are taken" in {
      allInts.WithUnfold.take(0) should be(Stream.empty)
    }

    "return the first n elements of a Stream" in {
      //this test will fail if trying to compare streams directly,
      //as the head and tails of streams are functions
      //and functions will be compared by their reference address
      //therefore streams will be coerced to lists before comparing them
      allInts.WithUnfold.take(4).toList should be (0 until 4)
    }
  }

  "Stream.WithUnfold::map " should {
    "map Empty to Empty" in {
      Empty.WithUnfold.map(_ => throw new Exception("test failed")) should be (Empty)
    }

    "map the elements of Stream lazily" in {
      allInts.WithUnfold.map(_ + 1).take(5).toList should be(List(1, 2, 3, 4, 5))
    }
  }

  "Stream.startsWith" should {
    "return true if Empty is passed as argument" in {
      allInts.startsWith(Empty) should be (true)
    }

    "return true if a prefix of the invoking stream is used as argument" in {
      allInts.startsWith(allInts.take(10)) should be (true)
    }

    "return false if an extension of the invoking stream is used as argument" in {
      allInts.take(10).startsWith(allInts) should be (false)
    }

    "return false if the the two streams do not agree on one element at some position" in {
      allInts.startsWith(Stream(1,2,3,7,5,6)) should be (false)
    }
  }

}
