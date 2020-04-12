package fpinscala
package monads

import java.util.concurrent.{ExecutorService, Future}

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._

import language.higherKinds


trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List(): List[A])) { case (ma, listOfResults) =>
      flatMap(listOfResults)(results => map(ma)(a => a::results))
    }

  //could use sequence(map(la)(f)), but that would require going through the list twice
  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] = {
    la.foldRight(unit(List(): List[B])) { case (a, listOfResults) =>
      flatMap(listOfResults)(results => map(f(a))(_::results))
    }
  }

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = {
    sequence(List.fill(n)(ma))
  }

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = { a =>
    flatMap(f(a))(g)
  }

  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = {
    compose(identity[M[A]], f)(ma)
  }

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(identity[M[A]])

  // Implement in terms of `join`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = {
    join(map(ma)(f))
  }

  //for Options, you cause the whole result to collapse to None
  //if p is not defined for at least one element in the list
  //for Lists, you evaluate several predicates p1,..., pn at once,
  //then you return a list of lists, each of which is filtered according
  //to one of the p1,...,pn
  //for State, you make the predicate depend on the state, other than on A
  def filterM[A](l: List[A])(p: A => M[Boolean]): M[List[A]] = {
    l.foldRight(unit(List(): List[A])) { case (a, mResults) =>
      flatMap(p(a)) { bool => if (bool) map(mResults)(a::_) else mResults }
    }
  }
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = {
      //using java futures does not make it easy to chain futures
      //Ideally one would want to avoid calling .get to wait for the result
      es: ExecutorService => f(pa(es).get())
    }
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A,B](maybeA: Option[A])(f: A => Option[B]): Option[B] = {
      maybeA.flatMap(f)
    }
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)
    override def flatMap[A,B](sa: Stream[A])(f: A => Stream[B]): Stream[B] =
      sa.flatMap(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A,B](la: List[A])(f: A => List[B]) = la.flatMap(f)
  }

  //State[S,A](run: S => (A, S))
  def stateMonad[S] = {
    new Monad[({type StateS[A] = State[S, A]})#StateS] {
      override def unit[A](a: =>A): State[S,A] = State { (s: S) => (a, s) }

      override def flatMap[A, B](sa: State[S,A])(f: A => State[S,B]): State[S,B] = {
        State[S,B] { s =>
          val (a, s1) = sa.run(s)
          f(a).run(s1)
        }
      }
    }
  }

  type Id[A] = A
  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: =>A): Id[A] = a
    override def flatMap[A,B](a: Id[A])(f: A => Id[B]): Id[B] = f(a)
  }

  def readerMonad[R] = new Monad[({type Reader[A] = R => A})#Reader] {
    override def unit[A](a: => A): R => A = (r: R) => a
    override def flatMap[A,B](aFromReader: R => A)(f: A => R => B): R => B = {
      r => f(aFromReader(r))(r)
    }
  }
}

