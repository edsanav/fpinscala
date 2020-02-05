package fpinscala.parsing

import java.util.regex.Pattern

import fpinscala.testing._

import language.higherKinds
import scala.util.matching.Regex

trait Parsers[Parser[+ _]] {
  self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  def slice[A](p: Parser[A]): Parser[String]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  /** In the event of an error, returns the error that occurred after consuming the most number of characters. */
  def furthest[A](p: Parser[A]): Parser[A]

  /** In the event of an error, returns the error that occurred most recently. */
  def latest[A](p: Parser[A]): Parser[A]


  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = p.flatMap(a => succeed(f(a)))

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = p.flatMap(
    (a: A) => p2.map((b: B) => f(a, b))
  )

  def map2ListComprehension[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
    for {a <- p; b <- p2} yield f(a, b)
  }

  def map2WithProduct[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = product(p, p2).map(f.tupled)

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = p.flatMap((a: A) => p2.map((b: B) => (a, b)))

  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List[A]())

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n <= 0) succeed(List[A]())
    else map2(p, listOfN(n - 1, p))(_ :: _)
  }

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)


  def succeed[A](a: A): Parser[A] // = string("").map(_ => a) // succedded parser (checks with empty string and returns imput)


  //def count[A](p:Parser[List[A]]):Parser[Int] // Too speciffic
  def count[A](p: Parser[Char]): Parser[Int] = many(p).map(_.size)


  def numAs[A]: Parser[List[String]] = "^[0-9]*".r.slice.flatMap(s => listOfN(s.toInt, "a"))

  def numAsBook[A]: Parser[List[String]] = for {
    digit <- "[0-9]+".r
    val n = digit.toInt // we really should catch exceptions thrown by toInt and convert to parse failure
    result <- listOfN(n, "a")
  } yield result

  /** Parser which consumes zero or more whitespace characters. */
  def whitespace: Parser[String] = "\\s*".r

  /** Parser which consumes 1 or more signed or not digits. */
  def digits: Parser[String] = "\\d+".r

  /** Parser which consumes 1 or more digits. */
  def letters: Parser[String] = "[a-zA-Z]+".r

  def rstrip[A, B](p: Parser[A], stripped: Parser[B]): Parser[A] = (p ** stripped.many).map(_._1)

  def lstrip[A, B](p: Parser[A], stripped: Parser[B]): Parser[A] = (stripped.many ** p).map(_._2)

  def strip[A, B](p: Parser[A], stripped: Parser[B]): Parser[A] = rstrip(lstrip(p, stripped), stripped)

  def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)

  def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)

  def middle[A, B, C](p: (A, B, C)): B = p._2

  def tuple3[A, B, C](p: Parser[A], p2: Parser[B], p3: Parser[C]): Parser[(A, B, C)] = ((p ** p2) ** p3).map(unbiasL)

  def between[A, B](p: Parser[A], p2: Parser[B]): Parser[(B, A, B)] = tuple3(p2, p, p2)

  def sequence[A](parsers: Parser[A]*): Parser[List[A]] = {
    parsers.toSeq.foldRight(succeed(List[A]()))((x, z) => x.map2(z)(_ :: _))
  }

  /* From solution */

  /** Sequences two parsers, ignoring the result of the first.
    * We wrap the ignored half in slice, since we don't care about its result. */
  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_, b) => b)

  /** Sequences two parsers, ignoring the result of the second.
    * We wrap the ignored half in slice, since we don't care about its result. */
  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a, b) => a)

  def opt[A](p: Parser[A]): Parser[Option[A]] =
    p.map(Some(_)) or succeed(None)

  /** Parser which consumes reluctantly until it encounters the given string. */
  def thru(s: String): Parser[String] = (".*?" + Pattern.quote(s)).r

  /** Unescaped string literals, like "foo" or "bar". */
  def quoted: Parser[String] = string("\"") *> thru("\"").map(_.dropRight(1))

  /** Unescaped or escaped string literals, like "An \n important \"Quotation\"" or "bar". */
  def escapedQuoted: Parser[String] =
  // rather annoying to write, left as an exercise
  // we'll just use quoted (unescaped literals) for now
    token(quoted label "string literal")

  /** C/Java style floating point literals, e.g .1, -1.0, 1e9, 1E-23, etc.
    * Result is left as a string to keep full precision
    */
  def doubleString: Parser[String] =
    token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  /** Floating point literals, converted to a `Double`. */
  def double: Parser[Double] =
    doubleString map (_.toDouble) label "double literal"

  /** Attempts `p` and strips trailing whitespace, usually used for the tokens of a grammar. */
  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whitespace

  /** Zero or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] = // use `Parser[Any]` since don't care about result type of separator
    sep1(p, p2) or succeed(List())

  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(p2 *> p))(_ :: _)

  /** Parses a sequence of left-associative binary operators with the same precedence. */
  def opL[A](p: Parser[A])(op: Parser[(A, A) => A]): Parser[A] =
    map2(p, many(op ** p))((h, t) => t.foldLeft(h)((a, b) => b._1(a, b._2)))

  /** Wraps `p` in start/stop delimiters. */
  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) =
    start *> p <* stop

  /** A parser that succeeds when given empty input. */
  def eof: Parser[String] =
    regex("\\z".r).label("unexpected trailing characters")

  /** The root of the grammar, expects no further input following `p`. */
  def root[A](p: Parser[A]): Parser[A] =
    p <* eof

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)

    def many: Parser[List[A]] = self.many(p)


    def flatMap[B](f: A => Parser[B]): Parser[B] =
      self.flatMap(p)(f)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def slice: Parser[String] = self.slice(p)

    def lstrip[B](p2: Parser[B]): Parser[A] = self.lstrip(p, p2)

    def rstrip[B](p2: Parser[B]): Parser[A] = self.rstrip(p, p2)

    def strip[B](p2: Parser[B]): Parser[A] = self.strip(p, p2)

    def between[B](p2: Parser[B]): Parser[(B, A, B)] = self.between(p, p2)

    def label(msg: String): Parser[A] = self.label(msg)(p)

    def scope(msg: String): Parser[A] = self.scope(msg)(p)

    def *>[B](p2: => Parser[B]) = self.skipL(p, p2)

    def <*(p2: => Parser[Any]) = self.skipR(p, p2)

    def token = self.token(p)

    def sep(separator: Parser[Any]) = self.sep(p, separator)

    def sep1(separator: Parser[Any]) = self.sep1(p, separator)

    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)

    def opL(op: Parser[(A, A) => A]): Parser[A] = self.opL(p)(op)

  }

  object Laws {

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, map(p)(a => a))(in)

    def succeedLaw[A](in: Gen[String])(in2: Gen[A]): Prop =
      Prop.forAll(in ** in2) { case (s, a) => run(succeed(a))(s) == Right(a) }

    def productLaw1[A](p1: Parser[A], p2: Parser[A], p3: Parser[A])(in: Gen[String]): Prop =
      equal(
        ((p1 ** p2) ** p3) map (unbiasL),
        (p1 ** (p2 ** p3)) map (unbiasR)
      )(in)

    def productLaw2[A, B](p1: Parser[A], p2: Parser[A])(f: A => B, g: A => B)(in: Gen[String]): Prop =
      equal(
        p1.map(f) ** p2.map(g),
        p1 ** p2 map { case (a, b) => (f(a), g(b)) }
      )(in)

  }


}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""

  def columnCaret = (" " * (col-1)) + "^"

}

case class ParseError(stack: List[(Location, String)] = List(),
                      otherFailures: List[ParseError] = List()) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_, s)).toList)

  def latestLoc: Option[Location] =
    latest map (_._1)

  def latest: Option[(Location, String)] =
    stack.lastOption

  /**
  Display collapsed error stack - any adjacent stack elements with the
  same location are combined on one line. For the bottommost error, we
  display the full line, with a caret pointing to the column of the error.
  Example:

  1.1 file 'companies.json'; array
  5.1 object
  5.2 key-value
  5.10 ':'

  { "MSFT" ; 24,
    */
  override def toString =
    if (stack.isEmpty) "no error message"
    else {
      val collapsed = collapseStack(stack)
      val context =
        collapsed.lastOption.map("\n\n" + _._1.currentLine).getOrElse("") +
          collapsed.lastOption.map("\n" + _._1.columnCaret).getOrElse("")
      collapsed.map { case (loc,msg) => loc.line.toString + "." + loc.col + " " + msg }.mkString("\n") +
        context
    }

  /* Builds a collapsed version of the given error stack -
   * messages at the same location have their messages merged,
   * separated by semicolons */
  def collapseStack(s: List[(Location,String)]): List[(Location,String)] =
    s.groupBy(_._1).
      mapValues(_.map(_._2).mkString("; ")).
      toList.sortBy(_._1.offset)

  def formatLoc(l: Location): String = l.line + "." + l.col

}

object Parsers {

}
