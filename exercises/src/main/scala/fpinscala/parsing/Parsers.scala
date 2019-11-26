package fpinscala.parsing

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

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = p.flatMap(a => succeed(f(a)))

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = p.flatMap(
    (a: A) => p2.map((b: B) => f(a, b))
  )

  def map2ListComprehension[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
    for {a <- p; b <- p2} yield f(a,b)
  }

  def map2WithProduct[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = product(p, p2).map(f.tupled)

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = p.flatMap((a:A) => p2.map((b: B) => (a,b)))

  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List[A]())

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n <= 0) succeed(List[A]())
    else map2(p, listOfN(n - 1, p))(_ :: _)
  }

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)


  def succeed[A](a: A): Parser[A] = string("").map(_ => a) // succedded parser (checks with empty string and returns imput)


  //def count[A](p:Parser[List[A]]):Parser[Int] // Too speciffic
  def count[A](p: Parser[Char]): Parser[Int] = many(p).map(_.size)

  def numAs[A]:Parser[List[String]] = "^[0-9]*".r.slice.flatMap(s => listOfN(s.toInt, "a"))

  def numAsBook[A]:Parser[List[String]] = for {
    digit <- "[0-9]+".r
    val n = digit.toInt // we really should catch exceptions thrown by toInt and convert to parse failure
    result <- listOfN(n, "a")
  } yield result

  /** Parser which consumes zero or more whitespace characters. */
  def whitespace: Parser[String] = "\\s*".r

  /** Parser which consumes 1 or more digits. */
  def digits: Parser[String] = "\\d+".r

  /** Parser which consumes 1 or more digits. */
  def letters: Parser[String] = "[a-zA-Z]+".r

  def rstrip[A, B](p:Parser[A], stripped:Parser[B]):Parser[A] = (p ** stripped.many).map(_._1)
  def lstrip[A, B](p:Parser[A], stripped:Parser[B]):Parser[A] = (stripped.many ** p ).map(_._2)
  def strip[A, B](p:Parser[A], stripped:Parser[B]):Parser[A] = rstrip(lstrip(p, stripped), stripped)

  def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)
  def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)
  def middle[A,B,C](p:(A,B,C)):B = p._2

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)

    def many: Parser[List[A]] = self.many(p)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def slice: Parser[String] = self.slice(p)

    def lstrip[B](p2: Parser[B]):Parser[A] = self.lstrip(p, p2)
    def rstrip[B](p2: Parser[B]):Parser[A] = self.rstrip(p, p2)
    def strip[B](p2: Parser[B]):Parser[A] = self.strip(p, p2)


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
}

case class ParseError(stack: List[(Location, String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}