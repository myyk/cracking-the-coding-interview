package phoneScreens

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import PhoneScreen3._

class PhoneScreen3Test extends FlatSpec with Matchers {

  "countWordFrequencies" should "work for the example case" in {
    val result = countWordFrequencies("apple cherry cherry banana apple apple")
    result("apple") shouldBe 3
    result("cherry") shouldBe 2
    result("banana") shouldBe 1
    result.size shouldBe 3
  }

  it should "work with strange white spaces" in {
    val result = countWordFrequencies("\n\t   apple \ncherry \t  cherry\n     \tbanana\t\t\t   \n\n\napple    apple  ")
    result("apple") shouldBe 3
    result("cherry") shouldBe 2
    result("banana") shouldBe 1
    println(result)
    result.size shouldBe 3
  }

  it should "work for empty strings" in {
    val result = countWordFrequencies("")
    result.size shouldBe 0
  }

  it should "work for small words" in {
    val result = countWordFrequencies("a c c b a a")
    result("a") shouldBe 3
    result("c") shouldBe 2
    result("b") shouldBe 1
    result.size shouldBe 3
  }

  "formattedWordFrequencies" should "format correctly with strange spacings" in {
    val result = formattedWordFrequencies("apple cherry cherry banana apple apple")
    result shouldBe "apple: 3\ncherry: 2\nbanana: 1"
  }
}