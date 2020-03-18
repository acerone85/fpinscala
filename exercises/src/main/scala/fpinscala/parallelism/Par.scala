package fpinscala.parallelism

import java.util.concurrent._

import org.joda.time.DateTime

import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.
  
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }

  //this causes .get to block on the underlying future, but
  //this the loss of parallelism is already an issue in map2.
  private case class TimedFuture[A](fromFuture: Future[A]) extends Future[(A, Long)] {
    def isDone = fromFuture.isDone()
    def get() = {
      val start = new DateTime()
      val result: A = fromFuture.get()
      val end = new DateTime()
      (result, end.getMillis - start.getMillis)
    }
    //I wonder if I can get rid of the code repetition here
    def get(timeout: Long, units: TimeUnit): (A, Long) = {
      val start = new DateTime()
      val result: A = fromFuture.get(timeout, units)
      val end = new DateTime()
      (result, end.getMillis - start.getMillis)
    }
    def isCancelled = fromFuture.isCancelled()
    def cancel(evenIfRunning: Boolean): Boolean = fromFuture.cancel(evenIfRunning)
  }

  //todo: but the contract
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = TimedFuture(a(es))
      val bf = b(es)
      new Future[C] {
        override def isDone: Boolean = af.isDone && bf.isDone

        def get: C = {
          val (a, _) = af.get()
          f(a, bf.get())
        }

        override def get(timeout: Long, units: TimeUnit): C = {
          val (a, t) = af.get()
          val b = bf.get(units.toMillis(timeout) - t, TimeUnit.MILLISECONDS)
          f(a, b)
        }

        override def cancel(mayInterruptIfRunning: Boolean): Boolean = {
          //cancel bf before af, as if af is cancelled while bf is running,
          //we may not be able to cancel bf.
          bf.cancel(mayInterruptIfRunning) && af.cancel(mayInterruptIfRunning)
        }

        override def isCancelled = af.isCancelled && bf.isCancelled
      }
    }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] { 
      def call = a(es).get
    })

  def asyncF[A, B](f: A => B): A => Par[B] = a => fork(unit(f(a)))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight[Par[List[A]]](unit(Nil)) { (nextPar, resultsSoFar) =>
      map2(nextPar, resultsSoFar)(_ :: _)
    }
  }

  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = {
    val parComps = l.map(asyncF(f))
    sequence(parComps)
  }

  def parFilter[A](l: List[A])(p: A => Boolean): Par[List[A]] = {
    map(parMap(l)(a => Some(a).filter(p)))(_.flatten)
  }

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => 
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
