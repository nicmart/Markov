/*
 * (c) 2015 Nicolò Martini
 *
 * http://nicolo.martini.io
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

package nicmart.markov

/**
 * Class Description
 */
class ChainStats[T, S](data: Map[T, Map[S, Int]]) {
  private def entropy(distribution: Map[S, Int]): Double = {
    val values = distribution.values
    val sum = values.sum
    val normalizedValues = values.map(_.toDouble / sum)

    -normalizedValues.map(prob => prob * math.log(prob)/math.log(2)).reduceLeft(_ + _)
  }

  def entropyMeanAndDeviation = {
    val entropies: Iterable[Double] = data.values.map(entropy(_))
    val size = entropies.size
    val mean = entropies.sum / size
    val variance = entropies.map(x => math.pow(x - mean, 2)).sum / size
    val deviation = math.sqrt(variance)
    (mean, deviation, entropies)
  }
}
