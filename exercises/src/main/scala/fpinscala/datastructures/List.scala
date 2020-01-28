package fpinscala.datastructures

import scala.annotation.tailrec

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
    @tailrec
    def reverseWithPartialResult(l: List[A], reversedSoFar: List[A]): List[A] = l match {
      case Nil => reversedSoFar
      case Cons(a, xs) => reverseWithPartialResult(xs, Cons(a, reversedSoFar))
    }

    reverseWithPartialResult(l, Nil)
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

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(a, xs) => Cons(f(a), map(xs)(f))
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match {
    case Nil => Nil
    case Cons(a, xs) => append(f(a), flatMap(xs)(f))
  }
}
