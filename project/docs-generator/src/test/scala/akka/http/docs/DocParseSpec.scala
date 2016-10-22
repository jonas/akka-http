package akka.http.docs

import org.scalatest.FunSuite
import scala.meta._

class DocParseSpec extends FunSuite {
  import DocParse._
  import DocTree._

  test("a") {
    val parsed = "'''italics'''".parse[Italic]
    assert(parsed == Parsed.Success(Italic("italic")))
  }
}
