/*
 * (c) 2015 Nicolò Martini
 *
 * http://nicolo.martini.io
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

package nicmart.markov.request

/**
 * Class Description
 */
case class Request(
    windowSize: Int,
    exponentialEntropy: Double,
    samePrefixPerSentence: Boolean,
    source: String,
    prefix: String
  ) {

}

object Request {

  def apply(args: Array[String]): Request = {
    val arguments = getArg(args) _

    Request(
      arguments(0, "3").toInt,
      arguments(1, "1.4").toDouble,
      arguments(2, "true").toBoolean,
      arguments(3, "mostrasprint"),
      arguments(4, "")
    )
  }

  private def getArg(args: Array[String])(position: Int, default: String) = {
    if (args.isDefinedAt(position)) args(position) else default
  }
}
