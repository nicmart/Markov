package nicmart.markov

import Helpers._
import IndexType._

object Markov {
  type TokenType = String

  def main(args: Array[String]): Unit = {
    val arguments = getArg(args) _
    val leftWindowSize = arguments(0, "2").toInt
    val rightWindowSize = arguments(1, "1").toInt
    val limit = arguments(2, "1000").toInt
    val file = arguments(3, "mostrasprint")
    val prefix = arguments(4, "")

    val sourceString: String = scala.io.Source.fromFile(file)
      .getLines().mkString(" ")

    val indexType = IndexType(leftWindowSize, rightWindowSize, Forward)
    val indexTypes = List(indexType)

    val engine = new MarkovEngine[String, TokenType](sourceString, leftWindowSize + rightWindowSize, indexTypes)

    val markovStream = prefix match {
      case "" => engine.stream(indexType)
      case _ => engine.stream(prefix, indexType)
    }

    val renderer = (new PunctuationWordStreamRenderer[TokenType])
      .andThen(CapitalizeAfterDot)
      .andThen(NewLineDecorator)

    val truncatedStream = markovStream.takeUntil(_ == ".", limit).take(10000)

    for (token <- renderer(truncatedStream)) {
      print(token.toString)
    }
  }

  def getArg(args: Array[String])(position: Int, default: String) = {
    if (args.isDefinedAt(position)) args(position) else default
  }
}
