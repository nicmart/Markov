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
trait KeyBuilder[T, S] extends (Iterable[T] => S) {
  def apply(elements: Iterable[T]): S
}

object KeyBuilder {
  implicit val stringsToString = new KeyBuilder[String, String] {
    def apply(elements: Iterable[String]) = elements.toSeq.mkString(" ")
  }

  implicit val charsToString = new KeyBuilder[Char, String] {
    def apply(elements: Iterable[Char]) = elements.mkString
  }
}