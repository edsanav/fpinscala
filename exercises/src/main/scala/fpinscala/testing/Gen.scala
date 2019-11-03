package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/


trait Prop {
  def &&(p: Prop): Prop = new Prop {
    def check = Prop.this.check && p.check
  }
//  def check: Either[(FailedCase, SuccessCount),SuccessCount]
  def check: Boolean
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

// case class State[S,A](run: S => (A,S))
case class Gen[A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] = flatMap((a:A)=>unit(f(a)))

  def map2[B,C](g:Gen[B])(f: (A,B)=>C):Gen[C] ={
    flatMap((aa:A)=>g.map((bb:B)=>f(aa,bb)))
  }

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap((a:A) =>f(a).sample))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  def listOfN(size:Gen[Int]): Gen[List[A]] = size.flatMap((n:Int)=>this.listOfN(n))

}

object Gen {

    val az:Seq[Char] = 'a' to 'z'

    def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

    def boolean:Gen[Boolean] = Gen(State(RNG.boolean))

    def listOfN[A](n: Int, g:Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

    def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(
      RNG.nonNegativeIntANSWER
    ).map((n:Int)=>start + n % (stopExclusive-start)))

    def choose(start: Double, stopExclusive: Double): Gen[Double] = Gen(State(
      RNG.double
    ).map((n:Double)=>start + n % (stopExclusive-start)))

    def choose2(start: Int, stopExclusive: Int):Gen[(Int,Int)] = {
       listOfN(2, choose(start, stopExclusive)).map((xs:List[Int])=>(xs.head,xs(1)))
    }

    def chooseCharacter:Gen[Char] = choose(0, az.length).map((n:Int)=>az(n))

    def stringN(n:Int):Gen[String] = listOfN(n, chooseCharacter).map((xs:List[Char])=>xs.mkString)

    def sequence[A](ls:List[Gen[A]]):Gen[List[A]] = {
      ls.foldRight(Gen.unit(List.empty[A]))((g:Gen[A], accum) => g.map2(accum)(_::_))
    }

    def union[A](g1:Gen[A], g2:Gen[A]):Gen[A] = Gen.boolean.flatMap(b=> if (b) g1 else g2)

    def weightedList[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
      def choicer(ls:List[Double], d:Double, index:Int):Int = {
        if (ls.length <=1 || ls.head >= d) index
        else choicer(ls.tail, d-ls.head, index+1)
      }
      val (genList, weights) = List(g1,g2).unzip
      choose(0D, weights.sum).flatMap(d => genList(choicer(weights, d, 0))))
    }

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    // As taken from the solution
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d =>
      if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }
}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}
//
//trait SGen[+A] {
//
//}

