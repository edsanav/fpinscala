package fpinscala.testing
import fpinscala.

case class State[S,A](run: S => (A,S))

trait Prop {
  import fpinscala.testing.Prop.{FailedCase, SuccessCount}
  def &&(p: Prop): Prop = new Prop {
    def check = Prop.this.check && p.check
  }
  def check: Boolean
//  def check: Either[(FailedCase, SuccessCount),SuccessCount]
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

object Gen {

  def listOf[A](a:Gen[List[A]]):List[A] = ???

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???



}

