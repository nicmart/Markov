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

  def andThen(renderer: StreamRenderer[String]): StreamRenderer[TokenType] = new StreamRenderer[TokenType] {
    def apply(tokens: Stream[TokenType]) = renderer(this(tokens))
  }
}

object StreamRenderer {
  val noSpaceBefore = Set(".", ",", ";", ":", "'", "?", "!", ")", "]", "}", "»")
  val noSpaceAfter = Set("'", "(", "[", "{", "«")
}

class PunctuationWordStreamRenderer[TokenType](
    noSpaceBefore: Set[String] = StreamRenderer.noSpaceBefore,
    noSpaceAfter: Set[String] = StreamRenderer.noSpaceAfter
  ) extends StreamRenderer[TokenType] {

  override def apply(tokens: Stream[TokenType]): Stream[String] = {
    tokens.slidingStream(2) flatMap { tokens: Stream[TokenType] =>
      val element1 = tokens(0).toString
      val element2 = tokens(1).toString
      if (noSpaceAfter.contains(element1) || noSpaceBefore.contains(element2)) Stream(element1)
      else Stream(element1, " ")
    }
  }
}

class PunctuationCharStreamRenderer[Char] extends StreamRenderer[Char] {

  def apply(tokens: Stream[Char]): Stream[String] = tokens map { _.toString }
}
