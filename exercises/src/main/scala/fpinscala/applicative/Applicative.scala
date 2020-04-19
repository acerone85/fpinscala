package fpinscala
package applicative

import monads.Functor
import state._
import State._
import StateUtil._ // defined at bottom of this file
import monoids._
import language.higherKinds
import language.implicitConversions

trait Applicative[F[_]] extends Functor[F] { self =>

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    apply(apply(unit(f.curried))(fa))(fb)
  }

  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A,B,C) => D): F[D] = {
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  }

  def _map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A,B,C) => D): F[D] = {
    val fPaired: (A, (B,C)) => D = { case (a, (b, c)) => f(a,b,c) }
    val fbc: F[(B, C)] = map2(fb, fc)((_,_))
    map2(fa, fbc)(fPaired)
  }

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa) {
    case (f, a) => f(a)
  }

  def unit[A](a: => A): F[A]

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(Nil: List[A])) { case (fa, fl) => map2(fa, fl)(_ :: _)}

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = sequence(as.map(f))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((_,_))

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: =>A): (F[A], G[A]) = (self.unit(a), G.unit(a))

      override def apply[A, B](f: (F[A => B], G[A => B]))(a: (F[A], G[A])): (F[B], G[B]) = {
        val (fab, gab) = f
        val (fa, ga) = a
        (self.apply(fab)(fa), G.apply(gab)(ga))
      }
    }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      override def map2[A,B,C](a: F[G[A]], b: F[G[B]])(f: (A,B) => C): F[G[C]] = {
        val liftedG: (G[A], G[B]) => G[C] = (ga, gb) => G.map2(ga, gb)(f)
        self.map2(a,b)(liftedG)
      }
    }

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = {
    ofa.foldLeft(unit(Map.empty[K, V])) { case (mapSoFarF, nextPair) =>
      val nextPairF = nextPair match {case (nextKey, nextValueF) =>
        map(nextValueF)(nextKey -> _)
      }

      map2(mapSoFarF, nextPairF)(_ + _)
    }
  }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
    new Monad[({type f[x] = Either[E, x]})#f] {
      override def unit[A](a: => A): Either[E,A] = Right(a)

      override def flatMap[A,B](fa: Either[E,A])(f: A => Either[E,B]): Either[E,B] = {
        fa match {
          case Left(e) => Left(e)
          case Right(a) => f(a)
        }
      }
    }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_],N[_]](
    implicit F: Monad[F],
    N: Monad[N],
    T: Traverse[N]
  ): Monad[({type f[x] = F[N[x]]})#f] = new Monad[({type f[x] = F[N[x]]})#f] {
    private[this] def mF: Monad[F] = implicitly[Monad[F]]
    private[this] def mN: Monad[N] = implicitly[Monad[N]]
    private[this] def tN: Traverse[N] = implicitly[Traverse[N]]

    override def unit[A](a: => A): F[N[A]] = {
      mF.unit(mN.unit(a))
    }

    override def map[A,B](fna: F[N[A]])(f: A => B): F[N[B]] = {
      mF.map(fna)(na => mN.map(na)(f))
    }

    override def join[A](fnfna: F[N[F[N[A]]]]): F[N[A]] = {
      //I could get directly a value of type F[F[N[A]] with only one traversal,
      //but I think this is more readable
      val ffnna: F[F[N[N[A]]]] = mF.map(fnfna)((nfna: N[F[N[A]]]) => tN.sequence[F, N[A]](nfna))
      val fnna: F[N[N[A]]] = mF.join(ffnna)

      mF.map(fnna)(nna => mN.join(nna))
    }
  }

}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] =
    new Applicative[({type f[x] = Validation[E,x]})#f] {
      override def unit[A](a: => A): Validation[E,A] = Success(a)

      override def map2[A,B,C](aV: Validation[E, A], bV: Validation[E,B])(f: (A, B) => C) = {
        aV match {
          case Failure(headA, tailA) => bV match {
            case Failure(headB, tailB) => Failure(headA, tailA ++ (headB +: tailB))
            case Success(b) => Failure(headA, tailA)
          }

          case Success(a) => bV match {
            case Failure(headB, tailB) => Failure(headB, tailB)
            case Success(b) => Success(f(a,b))
          }
        }
      }
    }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }

  type Id[A] = A

  val identityApplicative: Applicative[Id] = new Applicative[Id] {
    override def unit[A](a: => A): A = a

    override def apply[A,B](f: A => B)(a: A): B = f(a)
  }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  import Applicative._

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(identityApplicative)

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _  <- set(s2)
    } yield b).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = {
    val reversedList: List[A] = mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2

    mapAccum(fa, reversedList)((_, s) => (s.head, s.tail))._1
  }

  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z) { case (a,b) => ((), f(b,a))}._2

  override def foldRight[A,B](fa: F[A])(z: B)(f: (A,B) => B): B =
    foldLeft(reverse(fa))(z)((b, a) => f(a,b))

  def fuse[G[_]: Applicative, H[_]: Applicative, A, B](fa: F[A])(f: A => G[B], g: A => H[B]): (G[F[B]], H[F[B]]) = {
    type P[X] = (G[X], H[X])
    implicit val appGH: Applicative[P] =
      implicitly[Applicative[G]].product(implicitly[Applicative[H]])

    val fg: A => (G[B], H[B]) = a => (f(a), g(a))
    traverse[P,A,B](fa)(fg)
  }

  def compose[G[_]: Traverse]: Traverse[({type f[x] = F[G[x]]})#f] =
    new Traverse[({type f[X] = F[G[X]]})#f] {
      override def sequence[H[_]: Applicative, A](t: F[G[H[A]]]): H[F[G[A]]] = {
        val sequenceG: G[H[A]] => H[G[A]] = implicitly[Traverse[G]].sequence[H, A]
        self.sequence(self.map(t)((gh: G[H[A]]) => sequenceG(gh)))
      }
    }
}

object Traverse { self =>
  def unit[A, G[_]: Applicative](a: A): G[A] = implicitly[Applicative[G]].unit(a)
  def map2[A,B,C, G[_]: Applicative](ga: G[A], gb: G[B])(f: (A,B) => C) = implicitly[Applicative[G]].map2(ga, gb)(f)
  def map[A,B, G[_]: Applicative](ga: G[A])(f: A => B): G[B] = implicitly[Applicative[G]].map(ga)(f)

  val listTraverse: Traverse[List] = new Traverse[List] {

    override def sequence[G[_]: Applicative, A](t: List[G[A]]): G[List[A]] = {
      t.foldRight(unit(Nil: List[A])) { case (ga, gResult) =>
          map2(ga, gResult)(_::_)
      }
    }
  }

  val optionTraverse: Traverse[Option] = new Traverse[Option] {

    override def sequence[G[_]: Applicative, A](t: Option[G[A]]): G[Option[A]] = t match {
      case None => unit(None)
      case Some(ga) => self.map(ga)(a => Some(a))
    }


  }

  val treeTraverse: Traverse[Tree] = new Traverse[Tree] {

    override def sequence[G[_]: Applicative, A](t: Tree[G[A]]): G[Tree[A]] = {
      val tail: List[G[Tree[A]]] = t.tail.map(tga => sequence(tga))
      val tailG: G[List[Tree[A]]] = listTraverse.sequence(tail)

      map2(t.head, tailG)(Tree(_, _))
    }
  }
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
