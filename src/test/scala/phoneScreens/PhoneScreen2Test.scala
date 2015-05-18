package phoneScreens

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import PhoneScreen2._
import scala.util.Random
import org.scalatest.Inside
import org.scalatest.OptionValues

class PhoneScreen2Test extends FlatSpec with Matchers with OptionValues {
  "rotatedFind" should "find the index in a simple test cases" in {
    val array = Array(7, 8, 9, 10, 1, 2, 3, 4, 5, 6)
    for {
      (value, index) <- array.zipWithIndex
    } {
      rotatedFind(value, array) shouldBe Some(index)
    }
    rotatedFind(0, array) shouldBe None
  }

  "binarySearch" should "find the index in a simple test cases" in {
    val randomNumbers: Set[Int] = for {
      _ <- (0 to 1000).toSet[Int]
    } yield {
      Random.nextInt()
    }
    val array: Array[Int] = randomNumbers.toSeq.sorted.toArray

    for {
      (value, index) <- array.zipWithIndex
    } {
      binarySearch(value, array).value shouldBe index
    }

    for {
      _ <- (0 to 10000)
      val randomInt = Random.nextInt()
      if !randomNumbers.contains(randomInt)
    } {
      binarySearch(randomInt, array) shouldBe None
    }
  }

  it should "find the index in a singleton array" in {
    binarySearch(1, Array(1)).value shouldBe 0
    binarySearch(0, Array(1)) shouldBe None
  }
}