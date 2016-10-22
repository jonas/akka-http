package akka.http.docs

import scala.meta._
import scala.meta.tokenizers.Tokenize

case class DocTokenizer(input: Input, dialect: Dialect) {
  def tokenize(): IndexedSeq[DocToken] = {
    ???
  }
}

object DocTokenizer {
  /*
  def toTokenize: Tokenize = new Tokenize {
    def apply(input: Input, dialect: Dialect): Tokenized = {
      try {
        val tokenizer = new DocTokenizer(input, dialect)
        Tokenized.Success(tokenizer.tokenize())
      } catch {
        case details @ TokenizeException(pos, message) ⇒ Tokenized.Error(pos, message, details)
      }
    }
  }
  */
}
