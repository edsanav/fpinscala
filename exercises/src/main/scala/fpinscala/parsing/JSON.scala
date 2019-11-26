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

    type Quoted = (Char,String,Char)
    val spaces = char(' ').many.slice

    def quoted: Parser[Quoted] = (char('"') ** letters ** char('"')).map(unbiasL)

    def key:Parser[(Quoted, Char)] = quoted ** char(':').lstrip(whitespace)

    def entry:Parser[(String, JSON)] = key.map(_._1._2) ** jsonParser(P).strip(whitespace).rstrip(char(','))

    def jobject:Parser[JObject] = many(entry.strip(whitespace)).map(entry => JObject(entry.toMap))

    def root:Parser[JSON] = ???
    ???
  }
}