package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def toList: List[A] = foldRight(Nil: List[A])((a, xs) => a::xs)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = (n, this) match {
    case (0, _) => Stream.empty[A]
    case (_, Empty) => Stream.empty[A]
    case (_, Cons(head, tail)) => Stream.cons(head(), tail().take(n-1))
  }

  def drop(n: Int): Stream[A] = (n, this) match {
    case (0, _) => this
    case (_, Empty) => Stream.empty[A]
    case (_, Cons(_, tl)) => tl().drop(n-1)
  }

  /* takeWhile and other methods have multiple implementations.
   * A separate trait `Implementation` in the `Stream` object will define the interface
   * of methods that admit multiple implementations.
   * The different implementations of takeWhile will be provided in
   * the objects that extend `Implementation`,
   * and the method below will take the Implementation object to use as a parameter.
   * Two different implementation objects are provided, `FoldRight` and `NoFoldRight`.
   * The choice to use FoldRight as the default implementation for Stream methods
   * has been made, although this choice is arbitrary. */
  def takeWhile(p: A => Boolean, implementation: Implementation = FoldRight): Stream[A] =
    implementation.takeWhile(this)(p)

  def forAll(p: A => Boolean): Boolean = ???

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???

  sealed trait Implementation {
    private[Stream] def takeWhile[A](stream: Stream[A])(p: A => Boolean): Stream[A]
  }

  object NoFoldRight extends Implementation {
    override private[Stream] def takeWhile[A](stream: Stream[A])(p: A => Boolean): Stream[A] = {
      stream match {
        case Empty => Empty
        case Cons(hd, tl) => {
          if (p(hd())) Cons(hd, () => takeWhile(tl())(p)) else Empty
        }
      }
    }
  }

  object FoldRight extends Implementation {
    override private[Stream] def takeWhile[A](stream: Stream[A])(p: A => Boolean): Stream[A] =
      stream.foldRight(Empty: Stream[A]){ (a, partialStream) =>
        if (!p(a)) Empty
        else Cons(() => a, () => partialStream)
    }
  }
}