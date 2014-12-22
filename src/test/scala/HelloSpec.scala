import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import scala.collection.immutable.HashMap

class HelloSpec extends FlatSpec with ShouldMatchers {
  "Hello" should "have tests" in {
    true should be === true
  }

  "A map with a collection as key" should "check for collection values" in {
    val a = Array("a")
    val a2 = Array("a")
    val map = Map(a -> "test")

    val h = new collection.mutable.HashMap[Array[String], String] {
      protected override def elemEquals(key1: Array[String], key2: Array[String]): Boolean = {
        true
      }
    }

    map.contains(a2) should be === true
  }
}
