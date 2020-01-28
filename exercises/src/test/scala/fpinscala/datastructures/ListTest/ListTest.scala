package fpinscala.datastructures

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class ListTest extends AnyWordSpec with Matchers {

  "the function tail" should {
    "return an empty list when applied to an empty list" in {
      List.tail(Nil) should be (Nil)
    }

    "return an empty list when applied to a singleton list" in {
      List.tail(Cons(1, Nil)) should be (Nil)
    }

    "return the list without the head when applied to a list with multiple elements" in {
      List.tail(List(1,2,3,4,5)) should be (List(2,3,4,5))
    }
  }

  "the function reverse" should {
    "output an empty list when applied to an empty list" in {
      List.reverse(Nil) should be (Nil)
    }

    "correctly reverse a list of 5 elements" in {
      List.reverse(List(1,2,3,4,5)) should be (List(5,4,3,2,1))
    }
  }

  "the function drop" should {
    "not affect a list when a negative number of elements is dropped" in {
      List.drop(List(1,2,3), -1) should be(List(1,2,3))
    }

    "not affectr a lst when 0 elements are dropped from it" in {
      List.drop(List(1,2,3), 0) should be(List(1,2,3))
    }

    "return Nil if more elements than those included in the list are dropped" in {
      List.drop(List(1,2,3), 5) should be(Nil)
    }

    "drop the first 3 elements from a list of 5 elements" in {
      List.drop(List(1,2,3,4,5), 3) should be(List(4,5))
    }
  }

  "the function dropWhile" should {
    "drop all elements from a list when invoked with the always true predicate" in {
      List.dropWhile(List(1,2,3,4,5), (_: Int) => true) should be(Nil)
    }

    "not drop any element from a list when invoked with the always false predicate" in {
      List.dropWhile(List(1,2,3,4,5), (_: Int) => false) should be(List(1,2,3,4,5))
    }

    "not drop elements after the first element not satisfying the predicate is found" in {
      List.dropWhile(List(1,2,3,2,5), (n: Int) => n <= 2) should be(List(3,2,5))
    }
  }


  "the function init" should {
    "return an empty list when applied to an empty list" in {
      List.init(Nil) should be (Nil)
    }

    "return an empty list when applied to a singleton list" in {
      List.init(Cons(1, Nil)) should be (Nil)
    }

    "return the list without the last element when applied to a list with multiple elements" in {
      List.init(List(1,2,3,4,5)) should be (List(1,2,3,4))
    }
  }

  "the function length" should {
    "return 0 when applied to an empty list" in {
      List.length(Nil) should be(0)
    }

    "return 3 when applied to a list of 3 elements" in {
      List.length(List(1,2,3)) should be(3)
    }
  }

  "the function foldRight" should {
    "behave as the identity when using Nil as base value and Cons as reducer" in {
      List.foldRight(List(1,2,3,4,5), Nil: List[Int])( (x, xs) => Cons(x, xs)) should be(List(1,2,3,4,5))
    }

    "behave as mkString(\",\") - with trailin comma - " +
      "when using \"\" as base value and _ + \",\" + _ as reducer" in {
      (List.foldRight(List("a", "b", "c", "d", "e"), "")( (elem, string) => elem + "," + string )
        should be ("a,b,c,d,e,"))
    }
  }

  "the function tRecFoldRight" should {
    "behave as the identity when using Nil as base value and Cons as reducer" in {
      List.tRecFoldRight(List(1,2,3,4,5), Nil: List[Int])( (x, xs) => Cons(x, xs)) should be(List(1,2,3,4,5))
    }

    "behave as mkString(\",\") - with trailin comma - " +
      "when using \"\" as base value and _ + \",\" + _ as reducer" in {
      (List.tRecFoldRight(List("a", "b", "c", "d", "e"), "")( (elem, string) => elem + "," + string )
        should be ("a,b,c,d,e,"))
    }
  }

  "the function foldLeft" should {
    "behave as reverse when using Nil as base value and Cons as reducer" in {
      (List.foldLeft(List(1, 2, 3, 4, 5), Nil: List[Int])( (xs, x) => Cons(x, xs))
        should be (List(5, 4, 3, 2, 1)))
    }

    "behave as mkString(\",\") - with leading comma - " +
      "when using \"\" as base value and _ + \",\" + _ as reducer" in {
      (List.foldLeft(List("a", "b", "c", "d", "e"), "")((string, elem) => string + "," + elem)
        should be(",a,b,c,d,e"))
    }
  }

  "the function appendWithFold" should {
    "return the first input list if the second is Nil" in {
      List.appendWithFold(List(1, 2, 3), Nil: List[Int]) should be(List(1, 2, 3))
    }

    "return the second input list if the first is Nil" in {
      List.appendWithFold(Nil: List[Int], List(4, 5)) should be(List(4, 5))
    }

    "append two lists properly" in {
      List.appendWithFold(List(1, 2, 3), List(4, 5)) should be(List(1, 2, 3, 4, 5))
    }
  }
}
