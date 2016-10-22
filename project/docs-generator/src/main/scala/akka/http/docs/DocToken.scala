package akka.http.docs

import scala.meta._

trait DocToken {
  def input: Input
  def dialect: Dialect
  def start: Int
  def end: Int
  def pos: Position
}

object DocToken {
  case class Word(value: String)
  case class Section(tokens: Seq[DocToken])
}
