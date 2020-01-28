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

}
