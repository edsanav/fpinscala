package fpinscala

import ReferenceTypes._
import fpinscala.parsing._

import scala.util.matching.Regex

object ReferenceTypes {

  /** A parser is a kind of state action that can fail. */
  type Parser[+A] = Location => Result[A]

  trait Result[+A]
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]

}

object Reference extends Parsers[Parser] {

  def string(s: String): Parser[String] = (l:Location) => {
      if (l.input.startsWith(s)) Success(s, s.length)
      else Failure(l.toError(s"Expected :$s"))
    }

  def regex(r: Regex): Parser[String] = (l:Location) => {
    val matchOp = r.findFirstIn(l.input)
    matchOp match {
      case Some(s:String) => Success(s, s.length)
      case None => Failure(l.toError(s"No match for pattern: ${r.regex}"))
    }
  }

  def succeed[A](a: A): Parser[A] = (_:Location) => Success(a, 0)

  def slice[A](p: Parser[A]): Parser[String] = (l:Location) => {
    p(l) match {
      case Success(_:A, n) => Success(l.input.slice(l.offset, n), n)
      case _ => Failure(l.toError(s"Failed to slice"))
    }
  }

  def run[A](p: Parser[A])(s: String): Either[ParseError,A] = ???


  // consume no characters and succeed with the given value

  def or[A](p: Parser[A], p2: => Parser[A]): Parser[A] = ???

  def flatMap[A,B](f: Parser[A])(g: A => Parser[B]): Parser[B] = ???


  /* note, regex matching is 'all-or-nothing':
   * failures are uncommitted */




  def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???
  def label[A](msg: String)(p: Parser[A]): Parser[A] = ???

  def attempt[A](p: Parser[A]): Parser[A] = ???



  /* We provide an overridden version of `many` that accumulates
   * the list of results using a monolithic loop. This avoids
   * stack overflow errors for most grammars.
   */
  override def many[A](p: Parser[A]): Parser[List[A]] = ???

  /** In the event of an error, returns the error that occurred after consuming the most number of characters. */
  def furthest[A](p: Parser[A]): Parser[A] = ???

  /** In the event of an error, returns the error that occurred most recently. */
  def latest[A](p: Parser[A]): Parser[A] = ???

}