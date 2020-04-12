package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps
import fpinscala.testing.{Gen, Prop}

import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(b1: Boolean, b2: Boolean) = b1 || b2
    val zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(b1: Boolean, b2: Boolean) = b1 && b2
    def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(o1: Option[A], o2: Option[A]) = o2.orElse(o1)
    def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A) = g andThen f
    def zero: A => A =  (a: A) => a
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(a1: A, a2: A) = m.op(a2, a1)
    val zero: A = m.zero
  }

  def product[A,B](m1: Monoid[A], m2: Monoid[B]): Monoid[(A,B)] = new Monoid[(A,B)] {
    def op(ab1: (A,B), ab2: (A,B)): (A,B) = {
      val (a1, b1) = ab1
      val (a2, b2) = ab2

      (m1.op(a1, a2), m2.op(b1, b2))
    }

    def zero: (A,B) = (m1.zero, m2.zero)
  }


  import fpinscala.testing._
  import Prop._

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = forAll(
    for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z)
  ) { case (x, y, z) =>

    m.op(m.op(x, y), z) == m.op(x, m.op(y, z)) &&
    m.op(x, m.zero) == x &&
    m.op(m.zero, x) == x
  }
  
  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    concatenate(as.map(f), m)

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    def fSwap: (A, B) => B = (a: A, b: B) => f(b, a)
    foldMap(as, dual(endoMonoid[B]))(fSwap.curried)(z)
  }

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.isEmpty) m.zero
    else if (as.size == 1) f(as(0))
    else {
      val (firstHalf, secondHalf) = as.splitAt(as.size/2)
      m.op(foldMapV(firstHalf, m)(f), foldMapV(secondHalf, m)(f))
    }
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(p: Par[A], q: Par[A]) = Par.flatMap(p) {
      a1 => Par.map(q)(a2 => m.op(a1, a2))
    }

    def zero: Par[A] = Par.unit(m.zero)
  }


  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    foldMapV(v, par(m))(a => Par.lazyUnit(f(a)))
  }

  // Stub(c) + Stub("") = Stub(c + "") = Stub(c)
  // Part(l, w, r) + Stub("") = Part(l, w, r + "") = Part(l, w, r)
  // Stub("") + Stub(c) = Stub("" + c) = Stub(c)
  // Stub("") + Part(l, w, r) = Part("" + l, w, r) = Part(l, w, r)
  // Part("", 0, "") would not be a good candidate for zero:
  // Part("", 0, "") + Stub(l) = Part("", 0, l) != Stub(l)

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(wcLeft: WC, wcRight: WC): WC = {
      wcLeft match {
        case Stub(leftChars) => wcRight match {
          case Stub(rightChars) => Stub(leftChars ++ rightChars)
          case Part(lStub, words, rStub) => Part(leftChars ++ lStub, words, rStub)
        }
        case Part(leftLStub, leftWords, leftRStub) => wcRight match {
          case Stub(rightChars) => Part(leftLStub, leftWords, leftRStub ++ rightChars)
          case Part(_, rightWords, rightRStub) => Part(leftLStub, leftWords + rightWords + 1, rightRStub)
        }
      }
    }

    def zero: WC = Stub("")
  }

  def count(s: String): Int = {
    def stringToWC(char: Char): WC = char match {
      case ' ' => Part("", 0, "")
      case c => Stub(c.toString)
    }

    def toWordCount(s: String): Int = if (s.isEmpty) 0 else 1

    foldMap(s.toCharArray.toList, wcMonoid)(stringToWC) match {
      case Stub(chars) => toWordCount(chars)
      case Part(lChars, words, rChars) => words + toWordCount(lChars) + toWordCount(rChars)
    }
  }

  def productMonoid[A,B](m1: Monoid[A], m2: Monoid[B]): Monoid[(A, B)] =
    Monoid.product(m1, m2)

  def functionMonoid[A,B](m: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op (f: A => B, g: A => B) = (a: A) => m.op(f(a), g(a))

    def zero: A => B = (_: A) => m.zero
  }

  def mapMergeMonoid[K,V](m: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K,V]] {
    def op(mapLeft: Map[K,V], mapRight: Map[K,V]) = {
      (mapLeft.keySet ++ mapRight.keySet).map { k => k ->
        m.op(mapLeft.getOrElse(k, m.zero), mapRight.getOrElse(k, m.zero))
      }.toMap
    }

    def zero: Map[K,V] = Map.empty
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    IndexedSeqFoldable.foldMap(as)(a => Map(a -> 1))(mapMergeMonoid(intAddition))
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] = foldMap(as)(a => List(a))(listMonoid)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMap[A,B](as, mb)(f)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    concatenate(as.map(f))(mb)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B =
    concatenate(as.map(f))(mb)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(value) => f(value)
    case Branch(left, right) => mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
    case Leaf(value) => f(z, value)
    case Branch(left, right) => {
      //breaking down in two statements for code readability
      val foldLeftBranchResult = foldLeft(left)(z)(f)
      foldLeft(right)(foldLeftBranchResult)(f)
    }
  }
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match {
    case Leaf(value) => f(value, z)
    case Branch(left, right) => {
      val foldRightBranchResult = foldRight(right)(z)(f)
      foldRight(left)(foldRightBranchResult)(f)
    }
  }

}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as.map(f).foldLeft(mb.zero)(mb.op)
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    as.map(f(z,_)).getOrElse(z)
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    as.map(f(_,z)).getOrElse(z)
}

object Main extends App {
  import Monoid._
  import Prop._
  run(monoidLaws(intAddition, Gen.int))
}

