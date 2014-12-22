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
trait TokenExtractor[A, B] extends (A => Seq[B])

object TokenExtractor {
  implicit lazy val stringTokenExtractor: TokenExtractor[String, String] = new StringTokenExtractor
  implicit lazy val charTokenExtractor: TokenExtractor[String, Char] = CharTokenExtractor
}

class StringTokenExtractor(
    splitRegex: String = "((?=[^0-9a-zA-Zèéòçàùì ])|(?<=[^0-9a-zA-Zèéòçàùì ])| )",
    trim: Boolean = true,
    caseSensitive: Boolean = false
  ) extends TokenExtractor[String, String] {

  override def apply(source: String): Seq[String] = {
    val tokens = (if (caseSensitive) source else source.toLowerCase)
      .split(splitRegex)

    if (trim) {
      tokens.view.map(_.trim).filter(_.length > 0).force
    } else tokens
  }
}

object CharTokenExtractor extends TokenExtractor[String, Char] {
  override def apply(source: String): Seq[Char] = source
}