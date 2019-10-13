package fpinscala.state

import scala.annotation.tailrec


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

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    if (i == Int.MinValue) nonNegativeInt(rng2)
    else if (i < 0) (-i,rng2)
    else (i, rng2)
  }

  // more elegant
  def nonNegativeIntANSWER(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (a, rng2) = nonNegativeInt(rng)
    (a/(Int.MaxValue.toDouble+1), rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i,d), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d),rng2) = intDouble(rng)
    ((d,i),rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1,d2,d3),rng3)
  }

  @tailrec
  def rng_times[A](n:Int, accum:List[A])(ra:Rand[A])(rng:RNG): (List[A], RNG) = {
    if (n <= 0) (accum, rng)
    else {
      val (a, rng2) = ra(rng)
      rng_times(n-1, a::accum)(ra)(rng2)
    }
  }


  def rng_times_other[A](n:Int)(ra:Rand[A]): RNG => (List[A], RNG) = {
    @tailrec
    def inner(n: Int, accum:List[A])(ra:Rand[A])(rng:RNG): (List[A], RNG)= {
      if (n <= 0) (accum, rng)
      else {
        val (a, rng2) = ra(rng)
        inner(n-1, a::accum)(ra)(rng2)
      }
    }
    inner(n, List())(ra)(_)

  }

  def double3_other(rng:RNG): ((Double, Double, Double), RNG) = {
     val (l, rng2:RNG) = rng_times_other(3)(double)(rng)
    ((l.head, l(1),l(2)), rng2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    rng_times_other(count)(_.nextInt)(rng)
  }

  def doubleThroughMap(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt)(_/(Int.MaxValue.toDouble+1))(rng)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
