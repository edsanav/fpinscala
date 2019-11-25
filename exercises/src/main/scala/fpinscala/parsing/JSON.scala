package fpinscala.parsing


import language.higherKinds
import language.implicitConversions

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    def quoted: Parser[String] = (char('"') ** letters ** char('"')).map(unbiasL).map(middle)

    def entry:Parser[(String, JSON)] = (quoted ** char('.') ** jsonParser(P)).map(unbiasL).map(p => (p._1, p._3))

    val spaces = char(' ').many.slice

    def root:Parser[JSON] = ???

      ???
  }
}