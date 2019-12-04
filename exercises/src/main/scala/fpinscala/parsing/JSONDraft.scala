package fpinscala.parsing


import language.higherKinds
import language.implicitConversions

trait JSONDraft

// SEE https://www.json.org/
object JSONDraft {

  case object JNull extends JSONDraft

  case class JNumber(get: Double) extends JSONDraft

  case class JString(get: String) extends JSONDraft

  case class JBool(get: Boolean) extends JSONDraft

  case class JArray(get: IndexedSeq[JSONDraft]) extends JSONDraft

  case class JObject(get: Map[String, JSONDraft]) extends JSONDraft

  def jsonParser[Parser[+ _]](P: Parsers[Parser]): Parser[JSONDraft] = {
    import P._

    type Quoted = (Char, String, Char)
    val spaces = char(' ').many.slice

    def quoted: Parser[(String, String, String )] = regex("[^\\\\\"]+".r).between(string("\"")).strip(whitespace)

    def key: Parser[((String, String, String ), Char)] = quoted ** char(':').strip(whitespace)

    def value: Parser[JSONDraft] = (jnull | jnumber | jstring | jbool | jarray | jobject).between(whitespace).map(middle)

    def exponential: Parser[String] = sequence(string("e").or(string("E")), regex("[+-]?[0-9]+".r)).map(_.mkString)

    def jstring: Parser[JSONDraft] = quoted.map(p => JString(middle(p)))

    def jnull: Parser[JSONDraft] = string("null").map(_ => JNull)

    def jnumber: Parser[JSONDraft] = sequence(
      or(string("-"), succeed("")),
      or(sequence(string("0").or("[1-9]+".r), string(".")).map(_.mkString), succeed("")),
      digits,
      or(exponential, succeed(""))).map(_.mkString("").toDouble).map(JNumber)

    def jbool: Parser[JSONDraft] = or(
      string("true").map(_ => JBool(true)),
      string("false").map(_ => JBool(false))
    )

    def jarray: Parser[JSONDraft] = or(
      tuple3(string("["),whitespace,string("]")).map(_ => Vector[JSONDraft]()),
      tuple3(
        string("["),
        (many((value ** string(",")).map(_._1)) ** value).map(x => x._1.toVector :+ x._2),
        string("]")
      ).map(middle)
    ).map(JArray(_))


    def entry:Parser[(String, JSONDraft)] = key.map(_._1._2) ** value

    def jobject: Parser[JObject] = or(
      tuple3(string("{"),whitespace,string("}")).map(_ => Map[String, JSONDraft]()),
      tuple3(
        string("["),
        (many((entry ** string(",")).map(_._1)) ** entry).map(x => x._1 :+ x._2),
        string("]")
      ).map(middle)
    ).map(x => JObject(x.toMap))

    jobject
  }
}