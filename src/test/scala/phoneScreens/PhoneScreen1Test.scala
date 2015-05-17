package phoneScreens

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import PhoneScreen1._

class PhoneScreen1Test extends FlatSpec with Matchers {
  "findTimes1" should "work for a simple case" in {
    simpleFindTimes(findTimes1)
  }

  "findTimes2" should "work for a simple case" in {
    simpleFindTimes(findTimes2)
  }

  def simpleFindTimes(findTimesImpl: (Long, Set[String]) => (Seq[Long])): Unit = {
    serviceTimestamps = Map(
      "S1" -> Seq(1, 7, 100),
      "S2" -> Seq(2, 12)
    )

    val result = findTimesImpl(5, Set("S1", "S2"))
    // no duplicates
    result.toSet.size shouldBe result.size
    result.toSet shouldBe Set[Long](1, 2, 7)
  }
}