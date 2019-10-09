package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  def toListHelper: List[A] = {
    @annotation.tailrec
    def go(next: Stream[A], accum: List[A]): List[A] = next match {
      case Cons(h, t) => go(t(), h() :: accum)
      case _ => accum
    }

    go(this, List()).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n >= 1 => cons(h(), t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 1 => t().drop(n - 1)
    case Cons(_, t) if n == 1 => t()
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhileFold(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => {
      if (p(h)) cons(h, t.takeWhile(p))
      else empty
    })

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  def forAll2(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def mapUF[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }


  def takeUF(n: Int): Stream[A] =
    unfold((this, n)) {
      //      case (Cons(h,_), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n >= 1 => Some(h(), (t(), n - 1))
      case _ => None
    }

  def takeWhileUF(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B](s2: Stream[B]): Stream[(A, B)] =
    unfold((this, s2)) {
      case (Cons(h, t), Cons(h2, t2)) => Some((h(), h2()), (t(), t2()))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h, t), Cons(h2, t2)) => Some((Some(h()), Some(h2())), (t(), t2()))
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case _ => None

    }

  def filter[B](p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b.filter(p)) else b.filter(p))

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  //foldRight(s)({(a,b)=>cons(a,b)}:(A, =>Stream[B]) =>Stream[B])

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def startsWithNotEfficient[B](s: Stream[B]): Boolean =
    s.toList == zipWith(s).filter { case (a, b) => a == b }.map { case (_, b) => b }.toList

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll { // you need the zipAll so it allows blanks on the first stream, then filter, and do the forall
      case (h, h2) => h == h2
    }

  def tails: Stream[Stream[A]] =
    unfold(this){
      case Empty => None
      case s => Some(s, s.drop(1))
    } append(Stream())

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    foldRight((z, Stream()))(
      (a, p0) => {
        lazy val p1 = p0
        val b2 = f(a, p1._1)
        (b2, cons(b2, p1._2))
      }
    )._2
  }

}
case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream(n).append(from(n + 1))

  def from2(n: Int): Stream[Int] = Stream.cons(n, from2(n + 1))

  val fibs: Stream[Int] = {
    def go(n: Int, next: Int): Stream[Int] = {
      cons(n, go(next, n + next))
    }

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty
      case Some((head, next)) => cons(head, unfold(next)(f))
    }
  }

  val onesUF: Stream[Int] = {
    Stream.unfold(1)(_ => Some((1, 1)))
  }

  def constantUF[A](a: A): Stream[A] = Stream.unfold(a)(_ => Some((a, a)))

  def fromUF(a: Int): Stream[Int] = Stream.unfold(a)(n => Some((n, n + 1)))

  val fibsUf: Stream[Int] = {
    Stream.unfold((0, 1))((current_next) => Some((current_next._1, (current_next._2, current_next._1 + current_next._2))))
  }


}

