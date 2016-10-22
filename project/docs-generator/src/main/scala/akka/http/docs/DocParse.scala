package akka.http.docs

import scala.meta._
import scala.meta.parsers.Parse
import scala.meta.internal.parsers.ScalametaParser.toParse

object Parser {
  import fastparse.all._
  val parseA = P("a")
}

class DocParse(input: Input, dialect: Dialect) {
  println(input)
  def parseItalic(): DocTree.Italic = DocTree.Italic(".")
  def parseBold(): DocTree.Bold = DocTree.Bold(".")
  def parseCodeBlock(): DocTree.CodeBlock = DocTree.CodeBlock(".")
}

object DocParse {
  implicit lazy val parseItalic: Parse[DocTree.Italic] = toParse(_.parseItalic())
  implicit lazy val parseBold: Parse[DocTree.Bold] = toParse(_.parseBold())

  implicit lazy val parseCodeBlock: Parse[DocTree.CodeBlock] = toParse(_.parseCodeBlock())

  def toParse[T](fn: DocParse ⇒ T): Parse[T] = new Parse[T] {
    def apply(input: Input, dialect: Dialect): Parsed[T] = {
      try {
        val parser = new DocParse(input, dialect)
        Parsed.Success(fn(parser))
      } catch {
        case details @ TokenizeException(pos, message) ⇒
          Parsed.Error(pos, message, details)
        case details @ ParseException(pos, message) ⇒
          Parsed.Error(pos, message, details)
      }
    }
  }

}
