package fpinscala.errorhandling

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class EitherTest extends AnyWordSpec with Matchers {

  "Either::map" should {
    "map a Left value to itself" in {
      (Left(1): Either[_, Int]).map(_ + 1)  should be(Left(1))
    }

    "apply the input function to the value inside of a Right" in {
      Right(42).map(_ / 2) should be (Right(21))
    }
  }

  "Either::flatMap" should {
    "return the original argument when invoked by a Left" in {
      Left(1).flatMap((_: Any) => Left(2.0)) should be(Left(1))
    }

    "return a Left value when invoked by a Right using a function that returns a Left" in {
      Right(42).flatMap(n => Left(n/2)) should be(Left(21))
    }

    "return a Right value when invoked by a Right using a function that returns a Right" in {
      Right(42).flatMap(n => Right(n/2)) should be(Right(21))
    }
  }

  "Either::orElse" should {

    "return the Either that invokes the function if it is a Right" in {
      Right(42).orElse(Left(21)) should be(Right(42))
    }

    "return the input argument when invoked by a Left" in {
      Left(21).orElse(Right(42)) should be(Right(42))
    }

    "not evaluate the input argument when invoked by a Right" in {
      Right(42).orElse(throw new Exception("Test Failed")) should be(Right(42))
    }
  }

  "Either::map2" should {
    "return the Either that invokes the function if it is a Left" in {
      Left(42).map2(Left(21))((m: Int, n: Int) => m+n) should be(Left(42))
    }

    "return the Either passed as argument if it is a Left, and the function" +
      "is invoked by a Right" in {
      Right(42).map2(Left(21))((m: Int, n: Int) => m + n) should be(Left(21))
    }

    "apply the function f to the values of the Right that invokes map2 and the" +
      "Right passed as its argument" in {
      Right(42).map2(Right(21))(_ + _) should be(Right(63))
    }
  }

  "Either.traverse" should {
    "return Right(Nil) when applied to Nil" in {
      Either.traverse(Nil)((x: Any) => Left(x)) should be(Right(Nil))
    }

    "return the first Left produced by applying the function f to the elements " +
      "in the list, in the order they appear, if any" in {
      (Either.traverse(List(1,2,3,4))(n => if (n % 2 == 0) Left(n) else Right(n))
        should be (Left(2)))
    }

    "return a Right containing another list if the each element of the input " +
      "list returns a right once the function f is applied to it: each element " +
      "of the output list should be obtained by applying the function f to it, and stripping " +
      "the Right constructor away" in {
      (Either.traverse(List(1, 3, 5, 7))(n => if (n % 2 == 0) Left(n) else Right(n+1))
        should be(Right(List(2, 4, 6, 8))))
    }
  }

  "Either.sequence" should {
    "return the first Left contained in the input list, if any" in {
      (Either.sequence(List(Right(1), Left(2), Left(3)))
        should be(Left(2)))
    }

    "return a Right containing the elements of the input list " +
      "stripped away of the Right constructor, if the list contains no Left" in {
      (Either.sequence(List(Right(1), Right(2), Right(3)))
        should be(Right(List(1, 2, 3))))
    }
  }
}
