package nicmart.markov

import Helpers._

object Markov {
  type TokenType = String

  def main(args: Array[String]): Unit = {
    val limit = if (args.length > 0) args(0).toInt else 1000
    val windowSize = if (args.length > 1) args(1).toInt else 3
    val file = if (args.length > 2) args(2) else "mostrasprint"

    val sourceString: String = scala.io.Source.fromFile(file)
      .getLines().mkString(" ")

    val engine = new MarkovEngine[String, TokenType](windowSize)

    val markovStream = engine(sourceString)

    val renderer = (new PunctuationWordStreamRenderer[TokenType])
      .andThen(CapitalizeAfterDot)
      .andThen(NewLineDecorator)

    val truncatedStream = markovStream.takeUntil(_ == ".", limit).take(10000)

    for (token <- renderer(truncatedStream)) {
      print(token.toString)
    }
  }
}
