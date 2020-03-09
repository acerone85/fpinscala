package fpinscala.laziness

import Stream._

//adding sealed to trait Stream to ensure that pattern matching on Streams
// does not generate any warning at compile time for some methods
// (take, drop, zipWith)
sealed trait Stream[+A] {
  self =>

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean, implementation: Implementation = FoldRight): Boolean =
    implementation.exists(this)(p)

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
   * has been made, although this choice is arbitrary.
   */
  def takeWhile(p: A => Boolean, implementation: Implementation = FoldRight): Stream[A] =
    implementation.takeWhile(this)(p)

  def forall(p: A => Boolean, implementation: Implementation = FoldRight): Boolean =
    implementation.forall(this)(p)

  def headOption(implementation: Implementation): Option[A] = implementation.headOption(this)
  def headOption: Option[A] = FoldRight.headOption(this)
  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B, implementation: Implementation = FoldRight): Stream[B] =
    implementation.map(this)(f)

  def filter(p: A => Boolean, implementation: Implementation = FoldRight): Stream[A] =
    implementation.filter(this)(p)

  def append[B >: A](that: => Stream[B], implementation: Implementation = FoldRight): Stream[B] =
    implementation.append[A, B](this)(that)

  def flatMap[B](f: A => Stream[B], implementation: Implementation = FoldRight): Stream[B] =
    implementation.flatMap(this)(f)

  //the functions below should also be implemented with an implementation trait
  //however, because some of these functions (i.e. map) belong to the Implementation trait,
  //while others don't (take), we make a slightly different decision and wrap them inside a
  //WithUnfold object inside the Stream class
  object WithUnfold {
    def take(n: Int): Stream[A] = unfold[A, (Int, Stream[A])]((n, self)){ case (n, stream) =>
      if (n == 0) None
      else stream match {
        case Empty => None
        case Cons(head, tail) => Some(head(), (n-1, tail()))
      }
    }

    def map[B](f: A => B): Stream[B] = unfold[B, Stream[A]](self){
      case Empty => None
      case Cons(head, tail) => Some(f(head()), tail())
    }
  }

  def zipWith[B,C](that: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold[C, (Stream[A], Stream[B])]((this, that)){
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(thisHead, thisTail), Cons(thatHead, thatTail)) =>
        Some(f(thisHead(), thatHead()), (thisTail(), thatTail()))
    }
  }

  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] = {
    def tail[C](s: Stream[C]) = s match {
      case Empty => Empty
      case Cons(_, tail) => tail()
    }

    unfold[(Option[A],Option[B]), (Stream[A], Stream[B])]((this, that)){
      case (s1, s2) =>
        val t1 = tail(s1)
        val t2 = tail(s2)
        if (t1 == Empty && t2 == Empty) None
        else Some((s1.headOption, s2.headOption), (t1, t2))
    }
  }

  def startsWith[B](s: Stream[B]): Boolean =
    this.zipAll(s).takeWhile{ case (_, maybeB) => maybeB.isDefined}
      .forall { case (maybeA, maybeB) => maybeA.contains(maybeB.get)}

  def tails(): Stream[Stream[A]] = {
    def tailOption(stream: Stream[A]): Option[Stream[A]] = stream match {
      case Empty => None
      case Cons(_, tail) => Some(tail())
    }

    unfold[Stream[A], Option[Stream[A]]](Some(this)) {
      _.flatMap(stream => Some(stream, tailOption(stream)))
    }
  }

  //in the implementation below, we never access the tail of an
  // intermediate stream, hence each step of the scanRight takes
  // constant time. The asymptotic time of execution is O(|this|)
  def scanRight[B](init: B)(f: (A, B) => B): Stream[Stream[B]] = {
    this.foldRight(cons(cons(init, empty), empty): Cons[Cons[B]]) {
      case (a, partialStreams) =>
        val latestStream = partialStreams.h()
        val b = latestStream.h()
        cons(cons(f(a,b), latestStream), partialStreams)
    }
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Cons[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map{ case (a,ss) => cons(a, unfold(ss)(f)) }.getOrElse(empty)

  def constant[A](a: A): Stream[A] = unfold[A, Unit](())( _ => Some((a, ())))
  val ones: Stream[Int] = constant(1)
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  val fibs: Stream[Int] = {
    def genFib(m: Int, n: Int): Stream[Int] = cons(m, genFib(n, m + n))
    genFib(0,1)
  }

  object WithUnfoldInObject {
    def from(n: Int) = Stream.unfold[Int, Int](n)(n => Some(n, n + 1))

    val fibs = unfold[Int, (Int, Int)]((0, 1)) { case (m, n) => Some((m, (n, m + n))) }
  }
  sealed trait Implementation {
    private[Stream] def exists[A](stream: Stream[A])(p: A => Boolean): Boolean

    private[Stream] def forall[A](stream: Stream[A])(p: A => Boolean): Boolean =
      !exists(stream)(a => !p(a))

    private[Stream] def takeWhile[A](stream: Stream[A])(p: A => Boolean): Stream[A]

    private[Stream] def headOption[A](stream: Stream[A]): Option[A]

    private[Stream] def map[A, B](stream: Stream[A])(f: A => B): Stream[B]

    private[Stream] def filter[A](stream: Stream[A])(p: A => Boolean): Stream[A]

    private[Stream] def append[A, B >: A](stream: Stream[A])(otherStream: => Stream[B]): Stream[B]

    private[Stream] def flatMap[A, B](stream: Stream[A])(p: A => Stream[B]): Stream[B]
  }

  object NoFoldRight extends Implementation {

    override private[Stream] def exists[A](stream: Stream[A])(p: A => Boolean): Boolean =
      stream match {
        case Empty => false
        case Cons(head, tail) => p(head()) || exists(tail())(p)
      }

    override private[Stream] def takeWhile[A](stream: Stream[A])(p: A => Boolean): Stream[A] =
      stream match {
        case Empty => Empty
        case Cons(head, tail) => {
          if (p(head())) Cons(head, () => takeWhile(tail())(p)) else Empty
        }
      }

    override private[Stream] def headOption[A](stream: Stream[A]): Option[A] = stream match {
      case Empty => None
      case Cons(head, _) => Some(head())
    }

    override private[Stream] def map[A,B](stream: Stream[A])(f: A => B): Stream[B] = stream match {
      case Empty => Empty
      case Cons(head, tail) => Cons(() => f(head()), () => map(tail())(f))
    }

    override private[Stream] def filter[A](stream: Stream[A])(p: A => Boolean): Stream[A] = stream match {
      case Empty => Empty
      case Cons(head, tail) =>
        if (p(head()))
          Cons(head, () => filter(tail())(p))
        else
          filter(tail())(p)
    }

    override private[Stream] def append[A,B >: A](stream: Stream[A])(otherStream: => Stream[B]): Stream[B] =
      stream match {
        case Empty => otherStream
        case Cons(head, tail) => Cons(head, () => append[B, B](tail())(otherStream))
      }

    private[Stream] def flatMap[A, B](stream: Stream[A])(f: A => Stream[B]): Stream[B] = stream match {
      case Empty => Empty
      case Cons(head, tail) => append(f(head()))(flatMap(tail())(f))
    }
  }

  object FoldRight extends Implementation {

    override private[Stream] def exists[A](stream: Stream[A])(p: A => Boolean): Boolean =
      stream.foldRight(false)((a, b) => p(a) || b)


    override private[Stream] def takeWhile[A](stream: Stream[A])(p: A => Boolean): Stream[A] =
      stream.foldRight(Empty: Stream[A]){ (a, partialStream) =>
        if (!p(a)) Empty
        else Cons(() => a, () => partialStream)
    }

    override private[Stream] def headOption[A](stream: Stream[A]): Option[A] = {
      stream.foldRight(None: Option[A])((a, _) => Some(a))
    }

    override private[Stream] def map[A,B](stream: Stream[A])(f: A => B): Stream[B] = {
      stream.foldRight(Empty: Stream[B]) { (a, partialStream) =>
        Cons(() => f(a), () => partialStream)
      }
    }

    override private[Stream] def filter[A](stream: Stream[A])(p: A => Boolean): Stream[A] = {
      stream.foldRight(Empty: Stream[A]){ (a, partialStream) =>
        if (p(a))
          Cons(() => a, () => partialStream)
        else partialStream
      }
    }

    override private[Stream] def append[A, B >: A](stream: Stream[A])(otherStream: => Stream[B]): Stream[B] = {
      stream.foldRight(otherStream){ (a, partialStream) =>
        Cons(() => a, () => partialStream)
      }
    }

    private[Stream] def flatMap[A, B](stream: Stream[A])(f: A => Stream[B]): Stream[B] = {
      stream.foldRight(Empty: Stream[B]){ (a, partialStream) =>
        append(f(a))(partialStream)
      }
    }
  }
}