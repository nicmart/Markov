/*
 * (c) 2014 Nicolò Martini
 *
 * http://nicolo.martini.io
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

package nicmart.markov

import nicmart.{WeightedRandomDistribution, WeightedValue}
import nicmart.markov.Helpers._

import scala.collection.Map
import scala.util.Random

/**
 * Class Description
 */
class MarkovEngine[SourceType, TokenType]
    (windowSize: Int)
    (implicit tokenExtractor: TokenExtractor[SourceType, TokenType], keyBuilder: KeyBuilder[TokenType, String]) {

  def apply(source: SourceType): Stream[TokenType] = {
    val tokens = tokenExtractor(source)
    val prefix = startKeys(tokens).toStream
    val distributions = markovMap(counter(tokens))

    lazy val stream: Stream[TokenType] = prefix #::: stream.slidingStream(prefix.length).map { window =>
      distributions(keyBuilder(window.toSeq))()
    }

    stream
  }

  private def counter(tokens: Seq[TokenType]): OccurrenciesCounter[String, TokenType] = {
    val letterCounter = new OccurrenciesCounter[String, TokenType]

    for (window <- tokens.sliding(windowSize)) {
      //println(window.take(windowSize - 1).mkString(" "))
      letterCounter.addElement(keyBuilder(window.take(windowSize - 1)), window.last)
    }

    letterCounter
  }

  private def markovMap(counter: OccurrenciesCounter[String, TokenType]): Map[String, WeightedRandomDistribution[TokenType]] = {
    counter.getIndexes.mapValues(map => {
      val values = map.map(pair => WeightedValue(pair._1, pair._2))
      new WeightedRandomDistribution(values)
    })
  }

  private def startKeys(tokens: Seq[TokenType]) = {
    val index = Random.nextInt(tokens.length - windowSize)
    tokens.slice(index, index + windowSize - 1)
  }
}
