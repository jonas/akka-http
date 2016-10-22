package akka.http.docs

import scala.meta._
import java.io.{ File, PrintWriter }

object ExtractScalaDirectives {
  val SourceDir = new File("akka-http/src/main/scala/akka/http/scaladsl/server/directives")
  val ParadoxDir = new File("docs/src/main/paradox/scala/http/routing-dsl/directives")

  def main(args: Array[String]): Unit = {
    extractDirectivesFromTrait(new File(SourceDir, "BasicDirectives.scala"), "basic-directives")
  }

  def extractDirectivesFromTrait(file: File, dirName: String): Unit = {
    val content = scala.io.Source.fromFile(file).getLines.mkString("\n")
    val source = content.parse[Source].get
    val tokens = source.tokens
    val docDir = new File(ParadoxDir, dirName)

    def findComment(tree: Tree): Option[Token.Comment] = {
      tokens.reverse.find(commentBefore(tree)) match {
        //case comment: Option[Token.Comment] ⇒ comment
        case comment: Option[Token.Comment] ⇒ comment
        case _                              ⇒ None
      }
    }

    def undentComment(comment: Token.Comment) =
      comment.value.stripMargin('*').split('\n').map(_.trim).mkString("\n")

    def writeDocs(tree: Tree, paramType: Seq[Type.Param], name: Term.Name, args: Seq[Term.Param], restype: Option[Type]): Unit = {
      println(name)
      val comment = findComment(tree).map(undentComment)
      val pw = new PrintWriter(new File(docDir, name + ".md"))
      val paramTypeList = paramType match {
        case Nil ⇒ ""
        case _   ⇒ paramType.mkString("[", ", ", "]")
      }
      val argList = args match {
        case Nil ⇒ ""
        case _   ⇒ args.mkString("(", ", ", ")")
      }
      pw.write(
        s"""# $name
           |
           |## Signature
           |
           |```scala
           |def $name$paramTypeList$argList: ${restype.getOrElse("")}
           |```
           |
           |## Description
           |${comment.getOrElse("")}
           |
           |## Example
           |
           |```scala
           |...
           |```
           """.stripMargin)
      pw.close
    }

    source.traverse {
      case tree @ q"def $name[..$paramTypes](..$args): $restype = $body" ⇒
        writeDocs(tree, paramTypes, name, args, restype)

      case tree @ q"def $name[..$paramTypes]: $restype = $body" ⇒
        writeDocs(tree, paramTypes, name, Nil, restype)
    }
  }

  def commentBefore(tree: Tree)(token: Token): Boolean = {
    token.is[Token.Comment] && token.pos.end.offset < tree.pos.start.offset
  }
}
