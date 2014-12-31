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
import scala.collection.mutable
import scala.util.Random

/**
 * Class Description
 */
class MarkovEngine[SourceType, TokenType]
    (source: SourceType, windowSize: Int, indexTypes: Seq[IndexType])
    (implicit tokenExtractor: TokenExtractor[SourceType, TokenType], keyBuilder: KeyBuilder[TokenType, String]) {

  type Distribution = State => Option[Input]
  type State = Traversable[TokenType]
  type Input = Traversable[TokenType]

  private val automatons: IndexType => StateAutomaton[State, State] = indexTypes.map{
    indexType => (indexType, new SymbolStringAutomaton[TokenType](indexType.keyLength))
  }.toMap

  private val tokens: Seq[TokenType] = tokenExtractor(source)
  private val distributions: IndexType => Distribution = indicize

  private val chains: IndexType => DefaultStateAutomatonChain[State, Input] = indexTypes.map{
    indexType => (indexType, DefaultStateAutomatonChain[State, Input](automatons(indexType), distributions(indexType)))
  }.toMap


  private val defaultIndexType = indexTypes(0)

  def stream(prefix: Traversable[TokenType], indexType: IndexType): Stream[TokenType] = {
    val from: State = prefix.take(indexType.keyLength)
    val outputStream: Stream[Input] = chains(indexType).flattenOutputStream(from)
    prefix.toStream ++ outputStream.flatten
  }

  def test(from: State, indexType: IndexType): Unit = {
    chains(indexType).flattenOutputStream(from)
  }

  /**
   * Build a stream from a prefix of the same type of the source
   */
  def stream(prefix: SourceType, indexType: IndexType): Stream[TokenType] = {
    stream(generateStartSequence(prefix, indexType), indexType)
  }

  def generateStartSequence(prefix: SourceType, indexType: IndexType): Seq[TokenType] = {
    generateStartSequence(tokenExtractor(prefix), indexType)
  }

  def generateStartSequence(prefix: Traversable[TokenType], indexType: IndexType): Seq[TokenType] = {
    val candidates: Seq[Seq[TokenType]] =
      tokens.sliding(indexType.keyLength).filter(isPrefix(prefix.toSeq, _)).toSeq
    val length = candidates.length

    if (length == 0) generateStartSequence(indexType) else {
      candidates(Random.nextInt(length))
    }
  }

  def generateStartSequence(indexType: IndexType): Seq[TokenType] = {
    val index = Random.nextInt(tokens.length - windowSize)
    tokens.slice(index, index + indexType.keyLength)
  }

  private def isPrefix(a: Seq[TokenType], b: Seq[TokenType]) = {
    keyBuilder(a) == keyBuilder(b.take(a.length))
  }

  /**
   * Build the index of markov maps
   */
  private def indicize: IndexType => Distribution = {
    val counters = mutable.Map[IndexType, OccurrenciesCounter[String, Seq[TokenType]]]()

    indexTypes foreach {
      indexType => counters(indexType) = new OccurrenciesCounter[String, Seq[TokenType]]
    }

    // Build counters
    for (
        window <- tokens.sliding(windowSize);
        (indexType, counter) <- counters;
        (keys, values) = indexType.keysAndValues(window)
    ) {
      counter.addElement(keyBuilder(keys), values)
    }

    // Build distributions
    val distMap = counters mapValues (distributionFromCounter(_))

    (indexType: IndexType) => distMap.getOrElse(indexType, _ => None)
  }

  private def distributionFromCounter(
      counter: OccurrenciesCounter[String, Seq[TokenType]]
    ): State => Option[Input] = {
    val distributionsMap = counter.getIndexes.mapValues(map => {
      val values = map.map{case (value, weight) => WeightedValue(value, weight)}
      new WeightedRandomDistribution(values)
    })
    (from: State) => {
      val key = keyBuilder(from)
      if (distributionsMap.isDefinedAt(key)) Some(distributionsMap(key)()) else None
    }
  }
}
