package fpinscala.parsing

import fpinscala.testing._
import language.higherKinds

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p:Parser[A])(input: String):Either[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def listOfN[A](n:Int, p:Parser[A]):Parser[List[A]]

  def many[A](p: Parser[A]): Parser[List[A]]

  def map[A,B](a: Parser[A])(f: A => B): Parser[B]

  def flatMap[A,B](a: Parser[A])(f: A => Parser[B]): Parser[B]

  def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] = {
      flatMap(p)((a:A) => map(p2)((b:B) => f(a,b)))
    }

  def map2WithProduct[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] = map(product(p, p2))(f.tupled)

  def product[A,B](a:Parser[A],b:Parser[B]):Parser[(A,B)] = map2(a,b)((_,_))

  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_::_)

  def succeed[A](a: A):Parser[A] = string("").map(_ => a) // succedded parser (checks with empty string and returns imput)


  //def count[A](p:Parser[List[A]]):Parser[Int] // Too speciffic
  def count[A](p:Parser[Char]):Parser[Int] = map(many("a"))(_.size)

  def slice[A](p: Parser[A]): Parser[String]


  def or[A](s1:Parser[A], s2:Parser[A]): Parser[A]
  implicit def string(s:String): Parser[String]
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def and[A](s1:Parser[A], s2:Parser[A]): Parser[A]


  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)

    def &[B>:A](p2: Parser[B]): Parser[B] = self.and(p, p2)
    def and[B>:A](p2: Parser[B]): Parser[B] = self.and(p, p2)

    def **[B>:A](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def product[B>:A](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)


    def many:Parser[List[A]] = self.many(p)
    def flatMap[B](f:A=>Parser[B]):Parser[B] = self.flatMap(p)(f)
    def map[B](f:A=>B):Parser[B] = self.map(p)(f)

    def slice:Parser[String] = self.slice(p)

  }

  object Laws {

    def equal[A](p1:Parser[A], p2:Parser[A])(in:Gen[String]):Prop =
      Prop.forAll(in)(s=>run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =  equal(p, map(p)(a => a))(in)

    def succeedLaw[A](in:Gen[String])(in2:Gen[A]): Prop =
      Prop.forAll(in **in2 ){case (s, a) => run(succeed(a))(s) == Right(a)}
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}