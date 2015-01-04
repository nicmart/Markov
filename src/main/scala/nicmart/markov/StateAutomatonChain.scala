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
 * Class Description
 */
trait StateAutomatonChain[State, Input, T, TCollection <: Iterable[T]] extends MarkovChain[State, T, TCollection] {

  val automaton: StateAutomaton[State, Input]
  def distribution(from: State): Option[Input]

  def output(from: State, input: Input, to: State): TCollection

  override def next(from: State): Option[(State, TCollection)] = distribution(from) map { input =>
    val to = automaton.transition(from, input)
    //log("%s ==> %s ==> %s", from, input, to)
    (to, output(from, input, to))
  }

  private def toString[T](el: T) = el match {
    case e: Traversable[_] => e.mkString(" ")
    case _ => el.toString
  }

  private def log(format: String, from: State, input: Input, to: State) = {
    println(format.format(toString(from), toString(input), toString(to)))
  }
}

case class DefaultStateAutomatonChain[State, Input] (
    automaton: StateAutomaton[State, Input],
    private val distr: State => Option[Input]
  )  extends StateAutomatonChain[State, Input, Input, Seq[Input]] {

  def distribution(from: State): Option[Input] = distr(from)
  def output(from: State, input: Input, to: State): Seq[Input] = Seq(input)
}
