import sbt._
import sbt.Keys._

import com.lightbend.paradox.sbt.ParadoxPlugin

import java.io.File
import scala.io.Source
import scala.tools.nsc.reporters.{ Reporter, ConsoleReporter }
import scala.tools.nsc.doc.{ DocFactory, Settings }
import scala.tools.nsc.doc.doclet
import scala.collection.JavaConverters._

sealed trait Decl
case object DefDecl extends Decl
case object TermDecl extends Decl
case object TypeDecl extends Decl
case object UnknownDecl extends Decl

case class ScaladocComment(pkg: Option[String], symbol: String, text: String, lineNo: Int, declType: Decl)
case class ApiPage(pkg: Option[String], basename: String, comments: Seq[ScaladocComment])

import scala.util.control.NonFatal

/**
 * Path for scala library and compiler
 *
 * ref. https://github.com/twitter/util/blob/master/util-eval/src/main/scala/com/twitter/util/Eval.scala
 */
object ScalaPath {
  /*
   * For a given FQ classname, trick the resource finder into telling us the containing jar.
   */
  private def classPathOfClass(className: String) = try {
    val resource = className.split('.').mkString("/", "/", ".class")
    val path = getClass.getResource(resource).getPath
    if (path.indexOf("file:") >= 0) {
      val indexOfFile = path.indexOf("file:") + 5
      val indexOfSeparator = path.lastIndexOf('!')
      List(path.substring(indexOfFile, indexOfSeparator))
    } else {
      require(path.endsWith(resource))
      List(path.substring(0, path.length - resource.length + 1))
    }
  }
  private lazy val compilerPath = try {
    classPathOfClass("scala.tools.nsc.Interpreter")
  } catch {
    case NonFatal(e) =>
      throw new RuntimeException("Unable to load Scala interpreter from classpath (scala-compiler jar is missing?)", e)
  }

  private lazy val libPath = try {
    classPathOfClass("scala.ScalaObject")
  } catch {
    case NonFatal(e) =>
      throw new RuntimeException("Unable to load scala base object from classpath (scala-library jar is missing?)", e)
  }

  lazy val pathList = compilerPath ::: libPath
}

import scala.tools.nsc.doc.{ DocParser, Settings }
import java.io.File

/**
 * Extract examples from scala source.
 */
class ScaladocExtractor {

  private val settings = new Settings(println _)
  settings.bootclasspath.value = ScalaPath.pathList.mkString(File.pathSeparator)
  private val parser = new DocParser(settings)

  def extract(scalaSource: String): List[ScaladocComment] =
    parser.docDefs(scalaSource).map(toComment)

  private[this] def toComment(parsed: DocParser.Parsed) = {
    /*
    println(parsed.docDef.getClass)
    println(parsed.docDef.comment.getClass)
    //println(parsed.docDef.comment)
    println(parsed.docDef.comment.useCases)
    println(parsed.enclosing.getClass)
    //println(parsed.enclosing)
    println(s"isDef=${parsed.docDef.isDef}, isTerm=${parsed.docDef.isTerm}, isType=${parsed.docDef.isType}")
    println(s"symbol=${parsed.docDef.symbol}")
    println(s"definition=${parsed.docDef.definition} CLASS=${parsed.docDef.definition.getClass}")
    */
    //println(d)
    ScaladocComment(extractPkg(parsed), parsed.nameChain.lastOption.map(_.decode).getOrElse(""),
      parsed.docDef.comment.raw, parsed.docDef.comment.pos.line,
      if (parsed.docDef.isType) TypeDecl
      else if (parsed.docDef.isTerm) TermDecl
      else if (parsed.docDef.isDef) DefDecl
      else UnknownDecl)
  }

  private[this] def extractPkg(parsed: DocParser.Parsed): Option[String] = {
    val packages = parsed.enclosing
      .collect { case pkgDef: parser.PackageDef => pkgDef.pid.toString() }
      .filter(_ != "<empty>")
    packages match {
      case Nil => None
      case lst => Some(lst.mkString("."))
    }
  }
}

object MarkdownGenerator {

  val extractor = new ScaladocExtractor

  /**
   * Generates test source code from scala source file.
   */
  def apply(srcFile: File, srcEncoding: String): Seq[ApiPage] = {
    val src = Source.fromFile(srcFile, srcEncoding).mkString
    val basename = srcFile.getName // FilenameUtils.getBaseName(srcFile.getName)
    extractor.extract(src)
      //.flatMap(comment => ParsedDoctest(comment.pkg, comment.symbol, examples, comment.lineNo))
      .groupBy(_.pkg).map {
        case (pkg, examples) =>
          ApiPage(pkg, basename, examples)
      }
      .toSeq
  }
}

object ScalaDirectiveGeneratorPlugin extends AutoPlugin {
  object autoImport {
    val paradoxGenApiRef = taskKey[Seq[File]]("Generates API reference files.")
  }
  import autoImport._
  import ParadoxPlugin.autoImport.paradox

  override def requires = plugins.JvmPlugin
  override def trigger = noTrigger

  override def projectSettings: Seq[Setting[_]] = Def.settings(
    //dependencyClasspath in (Compile, doc) +=
    //projectDependencies in (Compile, doc) +=
    paradoxGenApiRef := {
      //(Keys.scalacOptions in (Compile, Keys.doc)).value ++ Opts.doc.externalAPI(Keys.apiMappings.value) ++ Seq("-doc-generator", generatorName)
      (managedSourceDirectories in Compile).value.headOption match {
        case None =>
          streams.value.log.warn("managedSourceDirectories in Compile is empty. Failed to generate API docs")
          Seq.empty
        case Some(testDir) =>
          streams.value.log.info(s"Generating into $testDir")

          val scaladocTests = doctestScaladocGenTests(
            (unmanagedSources in Compile).value,
            (scalacOptions in Compile).value
          )

          scaladocTests
            .filter(r => r.pkg == Some("akka.http.scaladsl.server.directives"))
            .foreach { r =>
              println(s"${r.basename}")
              r.comments.filter(_.text.contains("@group security")).foreach { c =>
                //val t = c.split("\\n").asScala.fi
                println(s" - ${c.symbol} (${c.declType}): $c")
              }
            }
            /*
            .groupBy(r => r.pkg -> r.basename)
            .flatMap {
              case ((pkg, basename), results) =>
                results.zipWithIndex.map {
                  case (result, idx) =>
                    val writeBasename = if (idx == 0) basename else basename + idx
                    val writeDir = pkg.fold(testDir)(_.split("\\.").foldLeft(testDir) { (a: File, e: String) => new File(a, e) })
                    val writeFile = new File(writeDir, writeBasename + "Doctest.scala")
                    IO.write(writeFile, result.markdown)
                    writeFile
                }
            }.toSeq
            */
          Seq.empty
      }
    },
    sourceGenerators in paradox := Seq.empty,
    sourceGenerators in paradox <+= paradoxGenApiRef
  )

  private def doctestScaladocGenTests(sources: Seq[File], scalacOptions: Seq[String]) = {
    val srcEncoding = findEncoding(scalacOptions).getOrElse("UTF-8")
    sources
      .filter(_.ext == "scala")
      .flatMap(MarkdownGenerator(_, srcEncoding))
  }

  private def findEncoding(scalacOptions: Seq[String]): Option[String] = {
    val index = scalacOptions.indexOf("-encoding")
    // The next element of "-encoding"
    if (index < 0 || index + 1 > scalacOptions.size - 1) None else Some(scalacOptions(index + 1))
  }
}
