package fpinscala.errorhandling

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class OptionTest extends AnyWordSpec with Matchers {

  "Option::map" should {
    "map None to None" in {
      (None: Option[Int]).map(_ + 1) should be(None)
    }

    "apply the function passed in input to the value by Some" in {
      Some(42).map(_ / 2) should be(Some(21))
    }
  }

  "Option::getOrElse" should {
    "return the default value when invoked by None" in {
      None.getOrElse(42) should be(42)
    }

    "return the value of a Some when invoked by Some" in {
      Some(42).getOrElse(0) should be(42)
    }

    "not evaluate the input argument when invoked by Some" in {
      Some(42).getOrElse(throw new Exception("Test fails")) should be(42)
    }
  }

  "Option::flatMap" should {
    "return None when invoked by None" in {
      None.flatMap(_ => Some(1)) should be(None)
    }

    "return None when invoked by a Some, using a function that returns None as parameter" in {
      Some(42).flatMap(_ => None) should be(None)
    }

    "apply the function passed as argument to the value of a Some, and flatten the resulting value" in {
      Some(42).flatMap(n => Some(n + 1)) should be(Some(43))
    }
  }

  "Option::orElse" should {
    "return the option specified in input when invoked by None" in {
      None.orElse(Some(42)) should be(Some(42))
    }

    "ignore the argument specified in input when invoked by a Some" in {
      Some(42).orElse(None) should be(Some(42))
    }

    "not evaluate the argument specified in input when invoked by a Some" in {
      Some(42).orElse(throw new Exception("Test Failed")) should be(Some(42))
    }
  }

  "Option::filter" should {
    "return None when invoked by None" in {
      None.filter(_ => true) should be(None)
    }

    "return None when invoked by Some using a false predicate" in {
      Some(42).filter(_ => false) should be(None)
    }

    "return the invoking object, when invoked by Some using a true predicate" in {
      Some(42).filter(_ => true) should be(Some(42))
    }
  }

  "Option.map2" should {
    "return None when passing two None as arguments" in {
      Option.map2(None, None) { case (_,_) => 0 } should be(None)
    }

    "return None when passing None as the first argument" in {
      Option.map2(None, Some(42)) { case (_,_) => 0 } should be(None)
    }

    "return None when passing None as the second argument" in {
      Option.map2(Some(42), None) { case (_,_) => 0 } should be(None)
    }

    "return Some(f(x,y)) when passing Some(x), Some(y) and f as arguments" in {
      Option.map2(Some(42), Some(21)) { case (x,y) => (x, y) } should be(Some(42, 21))
    }
  }

  "Option.sequence" should {
    "return Some(Nil) when applied to Nil" in {
      Option.sequence(Nil) should be(Some(Nil))
    }

    "return None when passing a list containing a None as argument" in {
      Option.sequence(Some(42)::None::Nil) should be(None)
    }

    "return Some(l) when passing a list containing only Some" in {
      Option.sequence(Some(42)::Some(21)::Some(7)::Nil) should be(Some(42::21::7::Nil))
    }
  }

  "Option.traverse" should {
    "return Some(Nil) when applied to Nil" in {
      Option.traverse(Nil)(_ => None) should be(Some(Nil))
    }

    "return None when using a function that returns None " +
      "if applied to one of the elements of the input list" in {
      (Option.traverse(42::21::7::Nil)(n => if(n==21) None else Some(n))
        should be(None))
    }

    "return Some(list) when using a function that returns Some " +
      "when applied to all the elements of the input list" in {
      (Option.traverse(42::21::7::Nil)(n => Some(n+1))
        should be(Some(43::22::8::Nil)))
    }
  }


}
