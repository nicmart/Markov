/*
 * (c) 2014 Nicolò Martini
 *
 * http://nicolo.martini.io
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

package nicmart.markov

/**
 * Trait Description
 */
trait TokenExtractor[A, B] extends (A => Seq[B]) {
  def apply(source: A): Seq[B]
}

object TokenExtractor {
  implicit lazy val stringTokenExtractor: TokenExtractor[String, String] = new StringTokenExtractor
  implicit lazy val charTokenExtractor: TokenExtractor[String, Char] = new CharTokenExtractor
  implicit lazy val linesTokenExtractor: TokenExtractor[Traversable[String], String] = {
    SequenceTokenExtractor(stringTokenExtractor)
  }
}

class StringTokenExtractor(
    splitRegex: String = "(?<=\\p{P})(?=\\P{P})|(?<=\\P{P})(?=\\p{P})| ",
    trim: Boolean = true,
    caseSensitive: Boolean = true
  ) extends TokenExtractor[String, String] {

  override def apply(source: String): Seq[String] = {
    val tokens = (if (caseSensitive) source else source.toLowerCase)
      .split(splitRegex)

    if (trim) {
      tokens.view.map(_.trim).filter(_.length > 0).force
    } else tokens
  }
}

class CharTokenExtractor(caseSensitive: Boolean = false) extends TokenExtractor[String, Char] {
  override def apply(source: String): Seq[Char] = if (caseSensitive) source else source.toLowerCase
}

case class SequenceTokenExtractor[A, B](extractor: TokenExtractor[A, B]) extends TokenExtractor[Traversable[A], B] {
  override def apply(source: Traversable[A]): Seq[B] = {
    source.flatMap(extractor(_)).toSeq
  }
}