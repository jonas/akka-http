package akka.http.docs

sealed trait DocTree
object DocTree {
  case class Ident(name: String) extends DocTree

  sealed trait Tag extends DocTree
  case class ConstructorTag(tree: Seq[DocTree]) extends Tag
  case class ReturnTag(tree: Seq[DocTree]) extends Tag
  case class ParamTag(ident: DocTree.Ident, tree: DocTree)
  case class TParamTag(ident: DocTree.Ident, tree: DocTree)
  case class ThrowsTag(ident: DocTree.Ident, tree: DocTree)
  // http://docs.scala-lang.org/overviews/scaladoc/for-library-authors.html

  sealed trait InlineElement extends DocTree
  // ''text''
  case class Italic(text: String) extends InlineElement
  // '''text'''
  case class Bold(text: String) extends InlineElement
  // __text__
  case class Underline(text: String) extends InlineElement
  // `text`
  case class Monospace(text: String) extends InlineElement
  // ^text^
  case class Superscript(text: String) extends InlineElement
  // ,,text,,
  case class Subscript(text: String) extends InlineElement
  // [[scala.collection.Seq]] becomes a link to the corresponding entity like Seq.
  // The entity must be linked using fully-qualified names (scala.collection.Seq instead of Seq);
  case class EntityLink(ident: DocTree.Ident) extends InlineElement
  // [[http://scala-lang.org Scala web site]] becomes Scala web site.
  // The URL part must start with a scheme name (like http:) and must not contain white space.
  // The name part (Scala web site) is optional.
  case class ExternalLink(url: String, text: Option[String]) extends InlineElement

  sealed trait BlockElement extends DocTree
  // a blank line starts a new paragraph. Note that a "blank line" may contain a left margin * delimiter, as described above.
  case class Paragraph(tree: Seq[DocTree]) extends DocTree
  // enclose between {{{ and }}}. See rule above for determining left margin.
  case class CodeBlock(code: String) extends DocTree
  // =heading= defines a heading. Lower-level heading are obtained by using more = signs, like ===sub-heading===. A heading must be define on its own line.
  case class Heading(level: Int, text: String) extends DocTree
  // List block: A list block is a sequence of list items of the same style and level, uninterrupted by other block elements.

  // ``$ - item` becomes a list item ($ signifies the left margin of a line
  // (it doesn't appear in the code), note the space between the left margin
  // and the dash). More leading space characters are allowed between the left
  // margin and the dash: list items with the same number of leading spaces are
  // at the same level, more space creates sub-levels.
  case class UnorderedList(items: Seq[DocTree]) extends DocTree
  // Uses similar rules to the unordered item, but replace dash by one
  // of 1., I., i., A. or a. for various numbering styles.
  case class OrderedList(items: Seq[DocTree]) extends DocTree
  case class Html(items: Seq[DocTree]) extends DocTree
}
