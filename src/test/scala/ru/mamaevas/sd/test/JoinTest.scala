package ru.mamaevas.sd.test

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class JoinTest extends AnyFlatSpec with Matchers with LazyLogging {

  import Join._

  val (data1, res1) = Set((1, 2), (5, 100), (2, 3)) -> Set((1, 3), (5, 100))
  val (data2, res2) = Set((-5, 10), (15, 20), (10, 25), (100, 200)) -> Set((-5, 25), (100, 200))
  val (data3, res3) = Set.empty[(Int, Int)] -> Set.empty[(Int, Int)]
  val (data4, res4) =
    Set((75, 105), (-50, -20), (-30, 30), (50, 100), (-100, 0), (102, 130), (45, 45), (100, 100), (30, 30), (70, 80)) ->
      Set((-100, 30), (45, 45), (50, 130))

  "join" should "be fine with List data" in {
    join(data1.toList) shouldBe res1.toList
    join(data2.toList) shouldBe res2.toList
    join(data3.toList) shouldBe res3.toList
    join(data4.toList) shouldBe res4.toList
  }

  it should "be fine with Set data" in {
    join(data1) shouldBe res1
    join(data2) shouldBe res2
    join(data3) shouldBe res3
    join(data4) shouldBe res4
  }

}
