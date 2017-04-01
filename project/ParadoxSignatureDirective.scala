package akka

import java.io.{File, FileNotFoundException}

import sbt._
import Keys._
import com.lightbend.paradox._
import com.lightbend.paradox.markdown._
import com.lightbend.paradox.sbt.ParadoxPlugin.autoImport._
import org.pegdown.Printer
import org.pegdown.ast.{DirectiveNode, HtmlBlockNode, VerbatimNode, Visitor}

import scala.collection.JavaConverters._
import scala.io.{Codec, Source}

object ParadoxSupport {
  val paradoxLocalApiEnabled = CliOption("akka.paradox.localapi", false)

  val paradoxWithSignatureDirective = Seq(
    (paradoxProcessor in Compile) := {
      val _ = paradoxProcessor in Compile // touch old reference
      // FIXME: this is a HACK so far that copies stuff over from paradox
      // it would be better if the plugin has a way of specifying extra directives through normal sbt mechanisms
      // see https://github.com/lightbend/paradox/issues/35
      new ParadoxProcessor(writer =
        new Writer(serializerPlugins = context =>
          Seq(
          new ActiveLinkSerializer,
          new AnchorLinkSerializer,
          new DirectiveSerializer(Writer.defaultDirectives(context) :+
            new SignatureDirective(context.location.tree.label, msg => streams.value.log.warn(msg))
      ))))
    },
    paradoxProperties in Compile ++= paradoxLocalApiEnabled.ifTrue {
      val targetPath = (baseDirectory in ThisBuild).value / "target"
      val scaladocPath = targetPath / ("scala-" + scalaBinaryVersion.value) / "unidoc" / "index.html"
      val javadocPath = targetPath / "javaunidoc" / "index.html"

      def property(name: String, path: File, command: String) =
        if (path.exists)
          Map(name -> path.toURI.toString)
        else {
          streams.value.log.error(s"Run `$command` to generate docs for akka.paradox.localapi")
          Map.empty
        }

      property("scaladoc.akka.http.base_url", scaladocPath, "sbt unidoc") ++
        property("javadoc.akka.http.base_url", javadocPath, "sbt -Dakka.genjavadoc.enabled=true javaunidoc:doc")
    }.getOrElse(Map.empty)
  )

  class SignatureDirective(page: Page, logWarn: String => Unit) extends LeafBlockDirective("signature") {
    def render(node: DirectiveNode, visitor: Visitor, printer: Printer): Unit =
      try {
        val labels = node.attributes.values("identifier").asScala.map(_.toLowerCase())
        val file = node.source match {
          case direct: DirectiveNode.Source.Direct => new File(page.file.getParentFile, direct.value)
          case _                                   => sys.error("Source references are not supported")
        }

        val Signature = """\s*((def|val|type) (\w+)(?=[:(\[]).*)(\s+\=.*)""".r // stupid approximation to match a signature
        //println(s"Looking for signature regex '$Signature'")
        val text =
          Source.fromFile(file)(Codec.UTF8).getLines.collect {
            case line@Signature(signature, kind, l, definition) if labels contains l.toLowerCase() =>
              //println(s"Found label '$l' with sig '$full' in line $line")
              if (kind == "type") signature + definition
              else signature
          }.mkString("\n")

        if (text.trim.isEmpty) {
          logWarn(
            s"Did not find any signatures with one of those names [${labels.mkString(", ")}] in ${node.source} " +
            s"(was referenced from [${page.path}])")

          new HtmlBlockNode(s"""<div style="color: red;">[Broken signature inclusion [${labels.mkString(", ")}] to [${node.source}]</div>""").accept(visitor)
        } else {
          val lang = Option(node.attributes.value("type")).getOrElse(Snippet.language(file))
          new VerbatimNode(text, lang).accept(visitor)
        }
      } catch {
        case e: FileNotFoundException =>
          throw new SnipDirective.LinkException(s"Unknown snippet [${e.getMessage}] referenced from [${page.path}]")
      }
  }
}