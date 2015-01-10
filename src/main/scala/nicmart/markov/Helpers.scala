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
 * Object Description
 */
object Helpers {

  /**
   * Implement a stream-compatible sliding feature
   */
  implicit class ImprovedStream[T](stream: Stream[T]) {
    def slidingStream(size: Int, step: Int = 1): Stream[Stream[T]] = {
      val window = stream.take(size)
      if (window.length < size) Stream()
      else window #:: stream.drop(step).slidingStream(size, step)
    }

    def slidingPaddedStream(size: Int, position: Int = 0): Stream[Stream[Option[T]]] = {
      val prefix: Stream[Option[T]] = fixedLengthAndValueStream[Option[T]](None, position)
      val suffix: Stream[Option[T]] = fixedLengthAndValueStream[Option[T]](None, size - position - 1)

      val optionStream = stream.map(Some(_))

      (prefix #::: optionStream #::: suffix).slidingStream(size)
    }

    def takeUntil(predicate: T => Boolean, times: Int, include: Boolean): Stream[T] = {
      val countStream = stream.scanLeft(1) { (count, element) =>
        if (predicate(element)) count + 1
        else count
      }
      stream.zip(countStream)
        .takeWhile{ case (element, count) => count <= times }
        .map{ case(element, count) => element }
    }

    def takeUntil(symbol: T, times: Int = 1, include: Boolean = true): Stream[T] = {
      takeUntil(_ == symbol, times, include)
    }

    def sentenceStream(predicate: T => Boolean, maxSentenceLength: Int): Stream[Stream[T]] = {
      val index = stream.take(maxSentenceLength).indexWhere(predicate)
      val splitPoint = if (index >= 0) index + 1 else maxSentenceLength
      val (sentence, tail) = stream.splitAt(splitPoint)
      tail match {
        case Stream() => Stream(sentence)
        case _ => sentence #:: tail.sentenceStream(predicate, maxSentenceLength)
      }
    }

    def sentenceStream(endOfSentence: T, maxSentenceLength: Int = 1000): Stream[Stream[T]] = {
      sentenceStream(_ == endOfSentence, maxSentenceLength)
    }

    private def fixedLengthAndValueStream[U](value: U, length: Int): Stream[U] = {
      if (length == 0) Stream()
      else value #:: fixedLengthAndValueStream(value, length - 1)
    }
  }

  implicit class ImprovedMap[A, B](map: Map[A, B]) {
    def reindexBy[C](p: A => C): Map[C, Map[A, B]] = {
      val initialMap = Map[C, Map[A, B]]()
      map.foldLeft(initialMap){
        case (newMap, (oldIndex, value)) => {
          val newKey = p(oldIndex)
          val submap = if(newMap.isDefinedAt(newKey)) newMap(newKey) else Map[A, B]()
          newMap.updated(newKey, submap.updated(oldIndex, value))
        }
      }
    }
  }

  implicit class TraversableWithStats[T : Numeric](sequence: Traversable[T]) {
    lazy val entropy: Double = {
      val num = implicitly[Numeric[T]]
      val sum = num.toDouble(sequence.sum)
      sequence.view
        .map(num.toDouble(_) / sum)
        .map( x => -x * math.log(x)  / math.log(2))
        .sum
    }
  }

  implicit class MapWithStats[S, T: Numeric](map: Map[S, T])  {
    lazy val entropy: Double = map.values.entropy
  }

  /**
   * Given a stream passed by name, build an infinite stream composed by repetitions of the passed stream
   */
  def inifiniteStream[T](stream: => Stream[T]): Stream[T] = {
    stream #::: inifiniteStream(stream)
  }
}
