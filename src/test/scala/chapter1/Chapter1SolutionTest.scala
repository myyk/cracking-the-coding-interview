package chapter1

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import Chapter1Solution._

class Chapter1SolutionTest extends FlatSpec with Matchers {

  "Question1_1" should "a string has all unique characters" in {
    stringHasAllUniqueCharacters("") shouldBe true
    stringHasAllUniqueCharacters("  ") shouldBe false
    stringHasAllUniqueCharacters("a") shouldBe true
    stringHasAllUniqueCharacters("abc") shouldBe true
    stringHasAllUniqueCharacters("aA") shouldBe true
    stringHasAllUniqueCharacters("aaa") shouldBe false
    stringHasAllUniqueCharacters("abbc") shouldBe false
  }

  "Question1_3" should "two strings are permuations of each other" in {
    arePermutations("", "") shouldBe true
    arePermutations("a", "") shouldBe false
    arePermutations("", "a") shouldBe false
    arePermutations("a", "a") shouldBe true
    arePermutations("abc", "bac") shouldBe true
    arePermutations("aabc", "bac") shouldBe false
    arePermutations("abc", "bbac") shouldBe false
    arePermutations("abc", "xyz") shouldBe false
    arePermutations("abc", "a") shouldBe false
    arePermutations("a", "bca") shouldBe false
  }

  "Question1_4" should "inplace replacement" in {
    val chars = "Mr John Smith    ".toCharArray
    replaceSpaces(chars, 13)
    chars shouldBe "Mr%20John%20Smith".toCharArray
  }

  "Question1_5" should "compress strings" in {
    compressString("") shouldBe ""
    compressString("a") shouldBe "a"
    compressString("222222") shouldBe "26"
    compressString("aabcccccaaa") shouldBe "a2b1c5a3"
    compressString("abc") shouldBe "abc"
  }

  "Question1_6" should "rotate a square 3x3 matrix" in {
    val matrix: Array[Array[Long]] = Array(
      Array(1, 2, 3),
      Array(4, 5, 6),
      Array(7, 8, 9)
    )
    val matrixRotatedCCW: Array[Array[Long]] = Array(
      Array(7, 4, 1),
      Array(8, 5, 2),
      Array(9, 6, 3)
    )
    rotateSquareMatrix(matrix) shouldBe matrixRotatedCCW
  }

  it should "rotate a square 4x4 matrix" in {
    val matrix: Array[Array[Long]] = Array(
      Array( 1, 2, 3, 4),
      Array( 5, 6, 7, 8),
      Array( 9,10,11,12),
      Array(13,14,15,16)
    )
    val matrixRotatedCCW: Array[Array[Long]] = Array(
      Array(13, 9, 5, 1),
      Array(14,10, 6, 2),
      Array(15,11, 7, 3),
      Array(16,12, 8, 4)
    )
    rotateSquareMatrix(matrix) shouldBe matrixRotatedCCW
  }
}