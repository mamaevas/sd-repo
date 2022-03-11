package ru.mamaevas.sd.test

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class JoinTest extends AnyFlatSpec with Matchers with LazyLogging {

  import Join._

  val data = List(
    Set.empty[(Int, Int)] -> Set.empty[(Int, Int)],
    Set((1, 2), (5, 100), (2, 3)) -> Set((1, 3), (5, 100)),
    Set((-5, 10), (15, 20), (10, 25), (100, 200)) -> Set((-5, 25), (100, 200)),
    Set((75, 105), (-50, -20), (-30, 30), (50, 100), (-100, 0), (102, 130), (45, 45), (100, 100), (30, 30), (70, 80)) ->
      Set((-100, 30), (45, 45), (50, 130))
  )

  "join" should "be fine with List data" in {
    data.foreach {
      case (in, expected) => join(in.toList) shouldBe expected.toList
    }
  }

  it should "be fine with Set data" in {
    data.foreach {
      case (in, expected) => join(in) shouldBe expected
    }
  }

}
