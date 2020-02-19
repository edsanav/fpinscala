package fpinscala.monoids

import fpinscala.monoids.ListFoldable.foldLeft
import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps

import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid:Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String):String = a1 + a2
    val zero:String = ""
  }

  def listMonoid[A]:Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]):List[A] = a1 ++ a2
    val zero:List[A] = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero:Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero:Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero:Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero:Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] =a1 orElse a2

    def zero: Option[A] = None
  }

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero:A = m.zero
  }

  // Now we can have both monoids on hand:
  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]
  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: (A => A), g: (A => A)): A => A = f compose g // this is the same as (a:A) => f(g(a))
    val zero:A => A = (a:A) => a
  }

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    forAll(
        for {
        x <-gen
        y <-gen
        z <-gen
      }  yield (x,y,z)){case (x,y,z) => m.op(m.op(x,y),z) == m.op(x, m.op(y,z))} &&
    forAll(gen)( a=> m.op(a, m.zero) == a  &&  m.op(m.zero, a) == a)
  }

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    ???

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero){case (b,a) => m.op(b,f(a))}

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    //foldMap(as, endoMonoid[B])((a:A) => f(_, a))(z)
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))((a:A) =>(b:B) => f(b,a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.length >=2) {
      val (as1,as2) = as.splitAt(as.length/2)
      m.op(foldMapV(as1, m)(f), foldMapV(as2, m)(f))
    }else if(as.length ==1){
      f(as(0))
    }else{
      m.zero
    }
  }

  // Copied from solutions. Annotations: Monoids of Some/None -> easy to set 0 and merge them
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val mon: Monoid[Option[(Int, Int, Boolean)]] = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(a1: Option[(Int, Int, Boolean)], a2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] =
        (a1,a2) match {
          case (x, None) => x
          case (None, x) => x
          case (Some((x1,y1, p)), Some((x2,y2,q))) => Some( (x1 min x2, y1 max y2, p && q && y1 <=x2))
        }

      def zero: Option[(Int, Int, Boolean)] = None
      }

    foldMapV(ints, mon)( i => Some(i, i, true)).map(_._3).getOrElse(true)
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def zero = Par.unit(m.zero)
    def op(a: Par[A], b: Par[A]) = a.map2(b)(m.op)
  }

  // My naive solution...
  //  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
  //    val parM = par(m)
  //    Par.fork(
  //      if (v.length >=2) {
  //        val (v1,v2) = v.splitAt(v.length/2)
  //        parM.op(parFoldMap(v1, m)(f), parFoldMap(v2, m)(f))
  //      }else if(v.length ==1){
  //        Par.unit(f(v(0)))
  //      }else{
  //        parM.zero
  //      }
  //    )
  //  }

  // book solution
  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(v)(f).flatMap { bs => // first it converts As to Bs
      foldMapV(bs, par(m))(b => Par.lazyUnit(b)) // then it folds them in parallel
    }

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC = (a1,a2) match {
      case (Stub(c1), Stub(c2)) => Stub(c1+c2)
      case (Stub(c), Part(l, w, r)) => Part(c+l,w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r+c)
      case (Part(l1, w1, r1),Part(l2, w2, r2)) => Part(
        l1, w1+( if ((r1+l2).isEmpty) 0 else 1)+w2, r2
      )
    }

    def zero: WC = Stub("")
  }

  def count(s: String): Int = {
    def unstub(s: String) = s.length min 1
    foldMapV(s.toIndexedSeq, wcMonoid)((c:Char) => {
     if (c.isWhitespace) Part("",0,"")
      else Stub(c.toString)
    }) match {
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A,B)] {
      def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
      def zero:(A,B) = (A.zero,B.zero)
    }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      override def op(a1: A => B, a2: A => B): A => B = (a:A) => B.op(a1(a),a2(a))

      override def zero: A => B = (_:A) => B.zero
    }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K,V]()
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)))
        }
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    IndexedSeqFoldable.foldMap(as)((a:A)=>Map((a,1)))(mapMergeMonoid(intAddition))
  //  foldMapV(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map(a -> 1)) (which is the same)
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = foldMap(as)((a:A) =>(b:B) => f(b,a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =  foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] = foldMap(as)((a:A) => List(a))(listMonoid)
  // equivalent to solutionfoldRight(as)(List[A]())(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B):B = as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B):B = as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
  foldLeft(as)(mb.zero)((b:B, a:A) => mb.op(b, f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  import Monoid._

  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B):B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B):B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Leaf(a) => f(a)
      case Branch(l,r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    as match {
      case Leaf(a) => f(z, a)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    as match {
      case Leaf(a) => f(a,z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }
}

object OptionFoldable extends Foldable[Option] {

  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =  as match {
    case None => mb.zero
    case Some(a:A) => f(a)
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match {
    case None => z
    case Some(a:A) => f(z,a)
  }

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
    case None => z
    case Some(a:A) => f(a,z)
  }
}

/*
import fpinscala.state._; import fpinscala.testing._; import fpinscala.testing.Gen._; import fpinscala.testing.Prop._

val myInts = Gen.choose(-10,10)
run(Monoid.monoidLaws(Monoid.intMultiplication, myInts))

val myWords =

 */
object examples {
  import fpinscala.state._; import fpinscala.testing._; import fpinscala.testing.Gen._; import fpinscala.testing.Prop._

  val myInts = Gen.choose(-10,10)

  val verifyInts = Monoid.monoidLaws(Monoid.intMultiplication, myInts)

  val myWords = SGen(n => Gen.stringN(n).listOfN(n).map(_.mkString(" ")))

  val sentences:Gen[String] = for {
    l <- Gen.choose(-10, 10)
    n <- Gen.choose(-50,50)
    sentence <- Gen.stringN(l).listOfN(n).map(_.mkString(" "))
  } yield sentence

}