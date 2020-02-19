package fpinscala.datastructures

import fpinscala.datastructures.Tree.{ImplementationFlag, WithFold, WithoutFold}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TreeTest extends AnyWordSpec with Matchers {
  val ImplementationFlags: Set[ImplementationFlag] = Set(WithFold, WithoutFold)

  ImplementationFlags.foreach { impFlag =>

    s"Tree.size (using $impFlag)" should {
      "return 1 on a leaf" in {
        Tree.size(Leaf(1), impFlag) should be(1)
      }

      "return 3 on a branch tree with two leaves" in {
        Tree.size(Branch(Leaf(1), Leaf(2)), impFlag) should be(3)
      }
    }

    s"Tree.maximum (using $impFlag)" should {
      "return the only value present in a leaf tree" in {
        Tree.maximum(Leaf(42), impFlag) should be(42)
      }

      "return the largest value of a leaf in a tree consisting of two leaves " in {
        Tree.maximum(Branch(Leaf(42), Leaf(21)), impFlag) should be(42)
        Tree.maximum(Branch(Leaf(21), Leaf(42)), impFlag) should be(42)
      }
    }

    s"Tree.depth (using $impFlag)" should {
      "return 0 on a Leaf tree" in {
        Tree.depth(Leaf(42), impFlag) should be(0)
      }

      "return 2 on a Branch three whose left Branch is a leaf, and whose " +
        "right Branch is a tree with two leaves" in {
        Tree.depth(Branch(Leaf(42), Branch(Leaf(1), Leaf(23))), impFlag) should be(2)
      }
    }

    s"Tree.map (using $impFlag)" should{
      "map a leaf into a leaf" in {
        Tree.map(Leaf(42), impFlag)(_ + 1) should be(Leaf(43))
      }

      "not alter the structure of a tree" in {
        Tree.map(Branch(Leaf(1), Leaf(2)), impFlag)(_ + 1) should be(Branch(Leaf(2), Leaf(3)))
      }
    }
  }
}
