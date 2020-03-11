package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def nonNegativeInt(implementation: Implementation)(rng: RNG): (Int, RNG) =
    implementation.nonNegativeInt(rng)

  def double(implementation: Implementation)(rng: RNG): (Double, RNG) =
    implementation.double(rng)

  def intDouble(implementation: Implementation)(rng: RNG): ((Int,Double), RNG) =
    implementation.intDouble(rng)

  def doubleInt(implementation: Implementation)(rng: RNG): ((Double,Int), RNG) =
    implementation.doubleInt(rng)

  def double3(implementation: Implementation)(rng: RNG): ((Double,Double,Double), RNG) =
    implementation.double3(rng)

  def ints(implementation: Implementation)(count: Int)(rng: RNG): (List[Int], RNG) =
    implementation.ints(count)(rng)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = (rng: RNG) =>{
    fs.foldRight[(List[A], RNG)]((List.empty, rng)) {
      case (rand, (partialList, lastRng)) =>
        val (nextElem, nextRng) = rand(lastRng)
        (nextElem :: partialList, nextRng)
    }
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = (rng: RNG) => {
    val (a, rngMid) = f(rng)
    g(a)(rngMid)
  }

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a,b)))
  }

  sealed trait Implementation {
    def nonNegativeInt: Rand[Int]
    def double: Rand[Double]
    def intDouble: Rand[(Int, Double)]
    def doubleInt: Rand[(Double, Int)]
    def double3: Rand[(Double, Double, Double)]
    def ints(count: Int): Rand[List[Int]]
  }

  object WithoutMap extends Implementation {
    override def nonNegativeInt: Rand[Int] = (rng: RNG) => {
      val (generated, nextRNG) = rng.nextInt
      (Math.max(Math.abs(generated), 0), nextRNG)
    }

    override def double: Rand[Double] = (rng: RNG) => {
      val (generated, nextRNG) = nonNegativeInt(rng)
      (generated.toDouble/Int.MaxValue, nextRNG)
    }

    override def intDouble: Rand[(Int, Double)] = (rng: RNG) => {
      val (generatedInt, intermediateRng) = int(rng)
      val (generatedDouble, finalRng) = double(intermediateRng)
      ((generatedInt, generatedDouble), finalRng)
    }

    override def doubleInt: Rand[(Double, Int)] = (rng: RNG) => {
      val (generatedPair, finalRng) = intDouble(rng)
      (generatedPair.swap, finalRng)
    }

    override def double3: Rand[(Double, Double, Double)] = (rng0: RNG) => {
      val (double0, rng1) = double(rng0)
      val (double1, rng2) = double(rng1)
      val (double2, rng3) = double(rng2)

      ((double0, double1, double2), rng3)
    }

    override def ints(count: Int): Rand[List[Int]] = (rng: RNG) => {
      (0 until count).foldLeft[(List[Int], RNG)]((List.empty, rng)){
        case ((partialList, lastRng), _) =>
          val (nextInt, nextRng) = int(lastRng)
          (nextInt::partialList, nextRng)
      }
    }
  }

  object WithHigherOrderFunctions  extends Implementation {
    override def nonNegativeInt: Rand[Int] =
      RNG.map(int)(n => Math.max(Math.abs(n), 0))

    override def double: Rand[Double] = RNG.map(nonNegativeInt)(n => n.toDouble/Int.MaxValue)

    override def intDouble: Rand[(Int, Double)] = {
      map2(int, double)((n, d) => (n, d))
    }

    override def doubleInt: Rand[(Double, Int)] = {
      map2(double, int)((d, n) => (d, n))
    }

    def double2: Rand[(Double, Double)] = {
      map2(double, double)((d1, d2) => (d1, d2))
    }

    override def double3: Rand[(Double, Double, Double)] = {
      map2(double, double2){ case (d1, (d2, d3)) => (d1, d2, d3) }
    }

    override def ints(n: Int): Rand[List[Int]] = {
      sequence(List.fill(n)(int))
    }
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = {
    State(s0 => {
      val (a, s1) = run(s0)
      (f(a), s1)
    })
  }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s0 => {
      val (a, s1) = run(s0)
      val (b, s2) = sb.run(s1)
      (f(a,b), s2)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s0 => {
      val (a, s1) = run(s0)
      f(a).run(s1)
    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  def unit[S,A](a: A): State[S, A] = State(s0 => (a, s0))
  def sequence[S, A](l: List[State[S,A]]): State[S, List[A]] = State { s0 =>
    val (finalList, finalState) = l.foldLeft[(List[A], S)]((List.empty, s0)) {
      case ((partialValues, lastState), nextStateMachine) =>
        val (nextValue, nextState) = nextStateMachine.run(lastState)
        (nextValue :: partialValues, nextState)
    }

    (finalList.reverse, finalState)
  }

  def fromInput(input: Input): State[Machine, Unit] = State { machine =>
    input match {
      case Coin if machine.locked && machine.candies > 0 =>
        ((), machine.copy(locked = false))
      case Turn if !machine.locked && machine.candies > 0 =>
        ((), machine.copy(
          locked = true,
          candies = machine.candies - 1,
          coins = machine.coins)
        )
      case _ => ( (), machine)
    }
  }

  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State { machine =>
    val (_, finalState) = sequence(inputs.map(fromInput)).run(machine)
    ((finalState.candies, finalState.coins), finalState)
  }
}
