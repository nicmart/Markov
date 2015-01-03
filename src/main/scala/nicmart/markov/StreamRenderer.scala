/*
 * (c) 2014 Nicolò Martini
 *
 * http://nicolo.martini.io
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

package nicmart.markov

import Helpers._

/**
 * Class Description
 */
trait StreamRenderer[-TokenType] {
  def apply(tokens: Stream[TokenType]): Stream[String]

  def andThen(renderer: StreamRenderer[String]): StreamRenderer[TokenType] = {
    val that = this
    new StreamRenderer[TokenType] { def apply(tokens: Stream[TokenType]) = renderer(that(tokens)) }
  }
}

object StreamRenderer {
  val noSpaceBeforeRegexp = """(\.|,|;|:|'|\?|!|\)|\]|\}|»|’).*"""
  val noSpaceAfterRegexp = """('|\(|\[|\{|«|’|#|@).*"""
  val capitalizeAfterRegexp = """(\.|\?|!).*"""
}

/**
 * Transform a token stream putting spaces between words, and checkgin for punctuations
 */
class PunctuationWordStreamRenderer[TokenType](
    noSpaceBefore: String = StreamRenderer.noSpaceBeforeRegexp,
    noSpaceAfter: String = StreamRenderer.noSpaceAfterRegexp
  ) extends StreamRenderer[TokenType] {

  override def apply(tokens: Stream[TokenType]): Stream[String] = {
    tokens.slidingPaddedStream(2, 0) flatMap {
      case Stream(Some(el1), Some(el2))
      if (el1.toString.matches(noSpaceAfter) || el2.toString.matches(noSpaceBefore)) => {
        Stream(el1.toString)
      }
      case Stream(Some(el), _) => Stream(el.toString, " ")
    }
  }
}

/**
 * Transform a char stream to a string stream
 */
class PunctuationCharStreamRenderer[Char] extends StreamRenderer[Char] {

  def apply(tokens: Stream[Char]): Stream[String] = tokens map { _.toString }
}

/**
 * Transform a stream putting new lines after dots
 */
object NewLineDecorator extends StreamRenderer[String] {
  def apply(tokens: Stream[String]): Stream[String] = {
    tokens.slidingPaddedStream(2, 1) map {
      case Stream(Some("."), Some(" ")) => "\n\n"
      case Stream(_, Some(element)) => element
    }
  }
}

/**
 * Transform a stream capitalizing words after a dot and a space
 */
object CapitalizeAfterDot extends StreamRenderer[String] {
  def apply(tokens: Stream[String]): Stream[String] = {
    tokens.slidingPaddedStream(3, 2) map {
      case Stream(_, None, Some(element)) => element.capitalize
      case Stream(Some(prefix), Some(" "), Some(element)) if prefix.matches(StreamRenderer.capitalizeAfterRegexp) => {
        element.capitalize
      }
      case Stream(_, _, Some(element)) => element
    }
  }
}
