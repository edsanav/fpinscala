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
case class Gen[A](sample: State[RNG,A])

object Gen {
    def unit[A](a: => A): Gen[A] = ???
    def choose(start: Int, stopExclusive: Int): Gen[Int] = ???
}

//object Gen {
//  def unit[A](a: => A): Gen[A] = ???
//}
//
//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}
//
//trait SGen[+A] {
//
//}

