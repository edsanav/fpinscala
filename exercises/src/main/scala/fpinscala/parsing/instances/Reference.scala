package fpinscala
package parsing

import ReferenceTypes._
import fpinscala.parsing._

import scala.util.matching.Regex

object ReferenceTypes {

  /** A parser is a kind of state action that can fail. */
  type Parser[+A] = ParseState => Result[A]

  /** `ParseState` wraps a `Location` and provides some extra
    * convenience functions. The sliceable parsers defined
    * in `Sliceable.scala` add an `isSliced` `Boolean` flag
    * to `ParseState`.
    */
  case class ParseState(loc: Location) {
    def advanceBy(numChars: Int): ParseState =
      copy(loc = loc.copy(offset = loc.offset + numChars))
    def input: String = loc.input.substring(loc.offset)
    def slice(n: Int) = loc.input.substring(loc.offset, loc.offset + n)
  }

  trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, c) => Failure(f(e), c)
      case _ => this
    }
    def uncommit: Result[A] = this match {
      case Failure(e,true) => Failure(e,false)
      case _ => this
    }
    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e,c) => Failure(e, c || isCommitted)
      case _=> this
    }
    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a,m) => Success(a,n+m)
      case _ => this
    }
    def extract: Either[ParseError,A] = this match {
      case Failure(e,_) => Left(e)
      case Success(a,_) => Right(a)
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError,
                     isCommitted: Boolean) extends Result[Nothing]
  /** Returns -1 if s1.startsWith(s2), otherwise returns the
    * first index where the two strings differed. If s2 is
    * longer than s1, returns s1.length. */
  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int = {
    var i = 0
    while (i < s1.length && i < s2.length) {
      if (s1.charAt(i+offset) != s2.charAt(i)) return i
      i += 1
    }
    if (s1.length-offset >= s2.length) -1
    else s1.length-offset
  }
}

object Reference extends Parsers[Parser] {

  def string(w: String): Parser[String] = {
    val msg = "'" + w + "'"
    s => {
      val i = firstNonmatchingIndex(s.loc.input, w, s.loc.offset)
      if (i == -1) // they matched
        Success(w, w.length)
      else
        Failure(s.loc.advanceBy(i).toError(msg), i != 0)
    }
  }

  def regex(r: Regex): Parser[String] = {
    val msg = "regex " + r
    s => {
    val matchOp = r.findPrefixOf(s.input)
    matchOp match {
      case None => Failure(s.loc.toError(msg), false)
      case Some(m: String) => Success(m, m.length)
    }
    }
  }
  def succeed[A](a: A): Parser[A] = s => Success(a, 0)

  def slice[A](p: Parser[A]): Parser[String] =
    s => p(s) match {
      case Success(_,n) => Success(s.slice(n),n)
      case f@Failure(_,_) => f
    }

  def run[A](p: Parser[A])(s: String): Either[ParseError,A] = p(ParseState(Location(s))).extract


  // consume no characters and succeed with the given value

  def or[A](x: Parser[A], y: => Parser[A]): Parser[A] =
    s => x(s) match {
      case Failure(e,false) => y(s)
      case r => r
    }

  def flatMap[A,B](f: Parser[A])(g: A => Parser[B]): Parser[B] =
    s => f(s) match {
      case Success(a,n) => g(a)(s.advanceBy(n))
        .addCommit(n != 0)
        .advanceSuccess(n)
      case f@Failure(_,_) => f
    }


  /* note, regex matching is 'all-or-nothing':
   * failures are uncommitted */




  def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.push(s.loc,msg))

  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.label(msg))

  def attempt[A](p: Parser[A]): Parser[A] =
    s => p(s).uncommit




  /* We provide an overridden version of `many` that accumulates
   * the list of results using a monolithic loop. This avoids
   * stack overflow errors for most grammars.
   */
  override def many[A](p: Parser[A]): Parser[List[A]] =
    s => {
      var nConsumed: Int = 0
      val buf = new collection.mutable.ListBuffer[A]
      def go(p: Parser[A], offset: Int): Result[List[A]] = {
        p(s.advanceBy(offset)) match {
          case Success(a,n) => buf += a; go(p, offset+n)
          case f@Failure(e,true) => f
          case Failure(e,_) => Success(buf.toList,offset)
        }
      }
      go(p, 0)
    }

  /** In the event of an error, returns the error that occurred after consuming the most number of characters. */
  def furthest[A](p: Parser[A]): Parser[A] = ???

  /** In the event of an error, returns the error that occurred most recently. */
  def latest[A](p: Parser[A]): Parser[A] = ???

}