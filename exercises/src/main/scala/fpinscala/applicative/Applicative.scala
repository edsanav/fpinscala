package fpinscala
package applicative

import monads.Functor
import state._
import State._
import StateUtil._
import monoids._

import language.higherKinds
import language.implicitConversions

trait Applicative[F[_]] extends Functor[F] {

  // map(fa)(a => f.curried(a):F[B => C] que en el global sirve para F[B => C](F[B])
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)(f.curried))(fb)//apply(apply(unit(f.curried))(fa))(fb)

  def map3[A,B,C,D](fa: F[A],
                    fb: F[B],
                    fc: F[C])(f: (A, B, C) => D): F[D] = apply(map2(fa,fb){case(a,b) => f.curried(a)(b)})(fc)
  // Alternatively, map3 = apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A,B,C,D,E](fa: F[A],
                      fb: F[B],
                      fc: F[C],
                      fd: F[D])(f: (A, B, C, D) => E): F[E] = apply(map3(fa,fb,fc){case(a,b,c) => f.curried(a)(b)(c)})(fd)
  // Alternatively, map4

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((f:(A => B), a:A) => f(a))

  def unit[A](a: => A): F[A]

  def map[A,B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(fa => fa)

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a:A,fb:F[List[B]]) => map2(f(a),fb)(_::_))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa,fb)((a,b) => (a,b))

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
      def applyMine[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) = fa match {
         case (xa:F[A], ya:G[A]) => (
           self.map2(xa, fab._1)((a,f) => f(a)),
           G.map2(ya, fab._2)((a,g) => g(a)),
         )
      }
      override def apply[A,B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])):(F[B],G[B]) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f]{
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] = {
        self.map2(fa,fb)((ga,gb)=> G.map2(ga, gb)((a,b)=>f(a,b)))
      }
    }
  }

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = ofa.foldLeft(unit(Map[K,V]())) {
    case (acc: F[Map[K, V]], (k: K, fv: F[V])) => map2(fv, acc)((v, kv) => kv + (k->v) )
  }

}

case class Tree[+A](head: A, tail: List[Tree[A]]) {
}

trait Monad[F[_]] extends Applicative[F] {
  // Minimal implementation of monad will need to implement unit and either flatMap or join and map
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f]  = new Monad[({type f[x] = Either[E, x]})#f] {
    def unit[A](a: => A):Either[E,A] = Right(a)

    //    override def flatMap[A,B](e:Either[E,A])(f: A => Either[E,B]) = e flatMap f easy one
    override def flatMap[A,B](e:Either[E,A])(f: A => Either[E,B]) = e match {
     case Right(a:A)  => f(a)
     case Left(e) => Left(e)
    }

  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
    Monad[({type f[x] = F[N[x]]})#f] = ???
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] =
    new Applicative[({type f[x] = Validation[E,x]})#f] {
      override def unit[A](a: => A): Validation[E, A] = Success(a)

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
        (fa, fb) match {
          case (Success(a), Success(b)) => Success(f(a, b))
          case (failA: Failure[E], failB: Failure[E]) => Failure(failA.head, failA.tail ++ Vector(failB.head) ++ failB.tail)
          case (_, e@Failure(_, _)) => e
          case (e@Failure(_, _), _) => e
        }
  }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  def map[A,B](fa: F[A])(f: A => B): F[B] = ???

  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _  <- set(s2)
    } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def zipWithIndexAlt[A](ta: F[A]): F[(A,Int)] =
    traverseS(ta)((a: A) => (for {
      i <- get[Int]
      _ <- set(i + 1)
    } yield (a, i))).run(0)._1

  def toListAlt[A](fa: F[A]): List[A] =
    traverseS(fa)((a: A) => (for {
      as <- get[List[A]]
      _ <- set(a :: as)
    } yield ())).run(List[A]())._2.reverse

  // we use the reverse of the list and then just build the final result by getting each head and setting the state
  // to the remaining of the list -> its tail
  def reverse[A](fa: F[A]): F[A] = mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a,b) => ((), f(b,a)))._2

  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)((a:A) => (f(a),g(a)))(G product(H)) //TODO review this
  //    (traverse(fa)(f), traverse(fa)(g))

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = ???
}

object Traverse {
  val listTraverse = new Traverse[List] {
    def traverse[G[_] : Applicative, A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List[B]()))((a, acc) => G.map2(f(a),acc)(_::_))
  }

  val optionTraverse =  new Traverse[Option] {
    def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      fa match {
        case Some(a) => G.unit(f(a))
        case None => G.unit(None)
      }
  }
  val treeTraverse = new Traverse[Tree] {
    def traverse[G[_] : Applicative, A, B](ta: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(
        f(ta.head),
        listTraverse.traverse(ta.tail)(t => this.traverse(t)(f)))(Tree(_,_))
  }

}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
