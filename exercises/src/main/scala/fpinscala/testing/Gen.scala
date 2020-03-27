package fpinscala.testing

import fpinscala.laziness.{Cons, Stream}
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.state.State.Rand

import scala.reflect.ClassTag

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/
sealed trait Result {
  def isFalsified: Boolean
  def prependErrorMessage(s: String): Result
  def appendErrorMessage(s: String): Result
  def continueWith(r: => Result): Result
  def recoverWith(r: Failed => Result): Result
}

object Passed extends Result {
  def isFalsified: Boolean = false
  def prependErrorMessage(s: String): Result = Passed
  def appendErrorMessage(s: String): Result = Passed
  def continueWith(r: => Result): Result = r
  def recoverWith(r: Failed => Result): Result = Passed
}

case class Failed(message: String, testsPassed: Int) extends Result {
  def isFalsified: Boolean = true
  def prependErrorMessage(s: String): Result = Failed(s"$s\n$message", testsPassed)
  def appendErrorMessage(s: String): Result = Failed(s"$message\n$s", testsPassed)
  def continueWith(r: => Result): Result = this
  def recoverWith(r: Failed => Result): Result = r(this)
}

case class Prop(run: (TestCases, RNG) => Result) {
  self =>
  def &&(that: Prop): Prop = Prop { (testCases, rng) =>
    self.run(testCases, rng)
      .prependErrorMessage("(")
      .appendErrorMessage("Failure happened in left branch\n)")
      .continueWith {
        that.run(testCases, rng)
          .prependErrorMessage("(")
          .appendErrorMessage("Failure happened in right branch\n)")
      }
  }

  //note that || connects Props. If prop1 tests for a property p1,
  // and prop2 tests for a property p2, then prop1 || prop2 does
  // not test for p1 || p2. In particular, prop1 || prop2 tests that
  // either all test cases satisfy p1, or either all test cases satisfy p2.
  // In contrast, a prop that tests for p1 || p2 would test that,
  // forall test cases, either p1 or p2 is satisfied. The latter
  // is impossible to specify in the current model.

  def ||(that: Prop): Prop = Prop { (testCases, rng)  =>
    self.run(testCases,rng).recoverWith { f: Failed =>
      that.run(testCases, rng)
        .prependErrorMessage(s"(\nLeft branch failed with result $f")
        .appendErrorMessage(s")")
    }
  }
}

object Prop {
  type TestCases = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop { (testCases, rng) =>
    stream(gen)(rng)
      .zipWith(Stream.from(0)){ case (a, b) => (a, b)}
      .take(testCases)
      .map { case (a, i) => try {
        if (f(a)) Passed else Failed(a.toString, i)
      } catch { case e: Throwable => Failed(buildMsg(a, e), i) }
      }.find(_.isFalsified)
      .getOrElse(Passed)
  }

  private def buildMsg[A](a: A, e: Throwable): String = {
    s"test case ${a} generated an exception ${e.getMessage}\n" +
      s"Stack trace: ${e.getStackTrace.mkString("\n")}"
  }
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def int: Gen[Int] = Gen(State(rng => rng.nextInt))
  def choose(start: Int, stopExclusive:Int): Gen[Int] =
      int.map(x => start + (x % (stopExclusive - start)) )

  def boolean: Gen[Boolean] = choose(0, 2).map(_ != 0)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean.flatMap(b => if (b) g1 else g2 )

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (gen1, d1) = g1
    val (gen2, d2) = g2
    val pinv = ((d1 / (d1 + d2)) * 100).toInt
    Gen.choose(0, pinv).flatMap( outcome => if (outcome < pinv) gen1 else gen2)
  }

  private def stream[A](r: Rand[A])(rng: RNG): Stream[A] = {
    val (a, rngNext) = r.run(rng)
    Stream.cons[A](a, stream(r)(rngNext))
  }
}

case class Gen[+A](r: State[RNG, A])  {
  def map[B](f: A => B): Gen[B] = Gen(r.map(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(r.flatMap(f(_).r))

  //pinv denotes the inverse of the probability of getting a None
  def toOption(pinv: Int): Gen[Option[A]] =
    Gen.weighted((Gen.unit[Option[A]](None), 1), (map(Some(_)), pinv - 1))

  def toOption: Gen[Option[A]] = toOption(100)

  def ground[B](implicit ev: A <:< Option[B]): Gen[B] = {
    map(_.getOrElse(this.ground))
  }

  def listOfN[A](n: Int): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(r)))

  def listOfAtMostN[A](n: Int): Gen[List[A]] =
    Gen.choose(0, n).flatMap(listOfN)

  def stream[A](rng: RNG): Stream[A] =
    Gen.stream(r)(rng)

  def unsized: SGen[A] = SGen(_ => this)
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] = SGen { n =>
    forSize(n).map(f)
  }

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen { n =>
    forSize(n).flatMap(a => f(a).forSize(n))
  }
}

object SGen {
  def listOf[A](gen: Gen[A]): SGen[List[A]] = SGen(gen.listOfN)
  
}

