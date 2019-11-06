package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}
import language.postfixOps
import language.implicitConversions

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/


//trait oldProp {
//  def &&(p: Prop): Prop = new Prop {
//    def check = Prop.this.check && p.check
//  }
////  def check: Either[(FailedCase, SuccessCount),SuccessCount]
//  def check: Boolean
//}


case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p:Prop):Prop = Prop(
    (max, n, rng) => {
      run(max, n,rng) match {
        case Passed => p.run(max, n,rng)
        case x => x
      }
    }
  )

  def ||(p:Prop): Prop = Prop(
    (max, n, rng) => {
      run(max, n,rng) match {
        case Falsified(msg, _) => p.tag(msg).run(max, n,rng)
        case x => x
      }
    }
  )

  def tag(msg:String):Prop = Prop(
    (max, n, rng) => {
      run(max, n,rng) match {
        case Falsified(failure, successes) => Falsified(s"$msg\n$failure", successes)
        case  x => x
      }
    }
  )





}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int


  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop(
    (n,rng) => {
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }.find(_.isFalsified).getOrElse(Passed)
    }
  )

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def apply(f: (TestCases,RNG) => Result): Prop =
    Prop { (_,n,rng) => f(n,rng) } // Alternate constructor

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }



}


// case class State[S,A](run: S => (A,S))
case class Gen[+A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] = flatMap((a:A)=>unit(f(a)))

  def map2[B,C](g:Gen[B])(f: (A,B)=>C):Gen[C] ={
    flatMap((aa:A)=>g.map((bb:B)=>f(aa,bb)))
  }

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap((a:A) =>f(a).sample))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  def listOfN(size:Gen[Int]): Gen[List[A]] = size.flatMap((n:Int)=>this.listOfN(n))

  def unsized: SGen[A] = SGen((_:Int) => this)

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
      choose(0D, weights.sum).flatMap(d => genList(choicer(weights, d, 0)))
    }

    def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
      // As taken from the solution
      /* The probability we should pull from `g1`. */
      val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

      Gen(State(RNG.double).flatMap(d =>
        if (d < g1Threshold) g1._1.sample else g2._1.sample))
    }

    def listOfN[A](g: Gen[A]): SGen[List[A]] = SGen((n:Int)=>g.listOfN(n))
}

case class SGen[+A](g: Int => Gen[A]){
  
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] = SGen(g(_).map(f))

  def map2[B,C](g2:Gen[B])(f: (A,B)=>C):SGen[C] = SGen(g(_).map2(g2)(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    SGen((n:Int) => g(n).flatMap( (a:A) => f(a)(n)))
  }

  def listOfN(size: Int): SGen[List[A]] = SGen((n:Int) => g(n).listOfN(size))


}