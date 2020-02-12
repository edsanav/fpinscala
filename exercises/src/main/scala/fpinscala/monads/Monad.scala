package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._
import language.higherKinds


trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List[A]())){case (a:M[A], acc:M[List[A]]) => map2(a,acc)(_::_)}

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List[B]())){case (a:M[A], acc:M[List[B]]) => map2(f(a),acc)(_::_)}

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = sequence(List.fill(n)(ma))

  def product[A,B](ma: M[A], mb:M[B]):M[(A,B)] = map2(ma, mb)((_,_))

  def filterMOtherNotEfficient[A](ms:List[A])(f:A => M[Boolean]): M[List[A]] = {
    map(traverse(ms)( (a:A) => product(unit(a),f(a))))( ls => ls.filter(_._2).map(_._1))
  }

    /*
  For `Par`, `filterM` filters a list, applying the functions in
  parallel; for `Option`, it filters a list, but allows
  the filtering function to fail and abort the filter
  computation; for `Gen`, it produces a generator for
  subsets of the input list, where the function `f` picks a
  'weight' for each element (in the form of a
  `Gen[Boolean]`)
  */
    def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
      ms match {
        case Nil => unit(Nil)
        case h :: t => flatMap(f(h))(b =>
          if (!b) filterM(t)(f) // if not condition, continue with the tail
          else map(filterM(t)(f))(h :: _)) // if condition, map head concatenated to whatever on tail
      }


  // Recursive version:
//  def _replicateM[A](n: Int, ma: F[A]): F[List[A]] =
//    if (n <= 0) unit(List[A]()) else map2(ma, replicateM(n - 1, ma))(_ :: _)

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = (a:A) => flatMap(f(a))(g)

  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = compose((_:Unit) => ma, f)()

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(ma => ma)

  // Implement in terms of `join`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = ???
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A,B](ma: Par[A])(f: A => Par[B]): Par[B] =
      ma flatMap f
  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    def unit[A](a: => A): P[A] = p.succeed(a)W
    override def flatMap[A,B](ma: P[A])(f: A => P[B]): P[B] =
      p.flatMap(ma)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A,B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    override def flatMap[A,B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma.flatMap(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A,B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }

  // Since `State` is a binary type constructor, we need to partially apply it
  // with the `S` type argument. Thus, it is not just one monad, but an entire
  // family of monads, one for each type `S`. One solution is to create a class
  // `StateMonads` that accepts the `S` type argument and then has a _type member_
  // for the fully applied `State[S, A]` type inside:
  class StateMonads[S] {
    type StateS[A] = State[S,A]

    // We can then declare the monad for the `StateS` type constructor:
    val monad = new Monad[StateS] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))
      override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
        st flatMap f
    }

  }

  //Alternative inlined version
  // https://underscore.io/blog/posts/2016/12/05/type-lambdas.html
  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  val idMonad: Monad[Id] = ???

  def readerMonad[R] = ???
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = ???
  def flatMap[B](f: A => Id[B]): Id[B] = ???
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = ???
    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = ???
  }
}

