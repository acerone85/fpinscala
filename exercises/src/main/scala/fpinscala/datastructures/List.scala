package fpinscala.datastructures

import scala.annotation.tailrec
import scala.concurrent.Future

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def empty[A]: List[A] = Nil

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil     //I'd prefer tail to return Option[List[A]], but the return type
                        // was enforced by the function definition
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    //I do not like the name setHead, as it suggests a mutable data structure.
    //.withHead would be a much better name
    case Nil => Nil
    case Cons(_, xs) => Cons(h, xs)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = n match {
    case m if m <= 0 => l
    case m => drop(tail(l), m - 1)
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    //below Cons(x, xs) would be better, but I do not like compiler warnings on variable shadowing
    case Cons(a, xs) if f(a) => dropWhile(tail(l), f)
    case Cons(a, xs) if !f(a) => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(a, xs) => Cons(a, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0){ case (_, partialLength) => 1 + partialLength }

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(a, xs) => foldLeft(xs, f(z, a))(f)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft[A, List[A]](l, Nil:List[A])((xs, x) => Cons(x, xs))
  }

  def tRecFoldRight[A,B](l: List[A], z: B)(f: (A,B) => B): B = {
    def swapped_f(b: B, a: A) = f(a,b)
    foldLeft(reverse(l), z)(swapped_f)
  }

  def appendWithFold[A](l1: List[A], l2: List[A]): List[A] =
    tRecFoldRight(l1, l2){ case (a, partiallyAppended) => Cons(a, partiallyAppended)}

  def flatten[A](seq: List[List[A]]): List[A] =
    foldRight[List[A], List[A]](seq, Nil) {
      case (l, partiallyFlattened) => append(l, partiallyFlattened)
    }

  def higherOrderMap[A, B, F[_]](l: List[A])(f: A => F[B])(mapF: (F[B], List[B]) => List[B]): List[B] = l match {
    case Nil => Nil
    case Cons(a, xs) => mapF(f(a), higherOrderMap(xs)(f)(mapF))
  }

  private[this] type Id[A] = A

  def map[A,B](l: List[A])(f: A => B): List[B] =
    higherOrderMap[A, B, Id](l)(f)(Cons(_, _))


  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    higherOrderMap(l)(f)(append)


  def filter[A](l: List[A])(p: A => Boolean): List[A] =
    flatMap(l)(a => if(p(a)) List(a) else Nil)

  def subsequenceNaive[A](l1: List[A], l2: List[A]): Boolean = {
    def subsequence_aux(x1: List[A], x2: List[A]): Boolean = {
      (x1, x2) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(a, xs1), Cons(b, xs2)) if b == a => subsequence_aux(xs1, xs2) || subsequence_aux(xs1, l2)
        case (Cons(_, xs1), _) => subsequence_aux(xs1, l2)
      }
    }

    subsequence_aux(l1,l2)
  }

  def subsequence[A](l1: List[A], l2: List[A]): Boolean = {
    var availableResults: Map[(List[A], List[A]), Boolean] = Map.empty


    def memoisedSubsequence(x1: List[A], x2: List[A]): Boolean = {
      availableResults.getOrElse((x1, x2), {
        val updatedAvailableResults = availableResults + {
          (x1, x2) -> { (x1, x2) match {
            case (_, Nil) =>
              true
            case (Nil, _) =>
              false
            case (Cons(a, xs1), Cons(b, xs2)) if b == a =>
              memoisedSubsequence(xs1, xs2) || memoisedSubsequence(xs1, l2)
            case (Cons(_, xs1), _) =>
              memoisedSubsequence(xs1, l2)
          }}
        }

        availableResults = updatedAvailableResults
        availableResults(x1, x2)  //will never cause an exception because availableResults(x1, x2) is defined above
      })
    }

    memoisedSubsequence(l1, l2)
  }

}
