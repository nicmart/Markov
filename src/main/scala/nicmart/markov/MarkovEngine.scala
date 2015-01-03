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

  type Distribution = (IndexType, State) => Option[Input]
  type State = Traversable[TokenType]
  type Input = Traversable[TokenType]
  type IndexedState = String

  /**
   * For each IndexType, define the state automaton
   */
  private val automatons: IndexType => StateAutomaton[State, State] = indexTypes.map{
    indexType => (indexType, new SymbolStringAutomaton[TokenType](indexType.keyLength))
  }.toMap


  private val tokens: Seq[TokenType] = tokenExtractor(source)
  private val distributions: Distribution = distributionFromMap(distributionsMap)

  private val chains: IndexType => DefaultStateAutomatonChain[State, Input] = indexTypes.map{
    indexType => (indexType, DefaultStateAutomatonChain[State, Input](automatons(indexType), (s: State) => distributions(indexType, s)))
  }.toMap


  private val defaultIndexType = indexTypes(0)

  def stream(prefix: Traversable[TokenType], indexType: IndexType): Stream[TokenType] = {
    val from: State = prefix.take(indexType.keyLength)
    val outputStream: Stream[Input] = chains(indexType).flattenOutputStream(from)
    prefix.toStream ++ outputStream.flatten
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
  private def distributionsMap: Map[(IndexType, IndexedState), WeightedRandomDistribution[Input]] = {
    // Build counter maps
    val elementsToCount = for (
        window <- tokens.sliding(windowSize);
        indexType <- indexTypes;
        (keys, values) = indexType.keysAndValues(window)
    ) yield ((indexType, keyBuilder(keys)), values)

    val countMaps = Counter.countPairs[(IndexType, String), Input](elementsToCount.toTraversable)

    countMaps.mapValues{ countMap: Map[Input, Int] =>
      new WeightedRandomDistribution(countMap.map{case (value, weight) => WeightedValue(value, weight)})
    }
  }

  private def distributionFromMap(
      randomDistribs: Map[(IndexType, IndexedState), WeightedRandomDistribution[Input]]
    ): Distribution = {
    (indexType: IndexType, from: State) => {
      val key = keyBuilder(from)
      if (randomDistribs.isDefinedAt((indexType, key))) Some(randomDistribs((indexType, key))()) else None
    }
  }
}
