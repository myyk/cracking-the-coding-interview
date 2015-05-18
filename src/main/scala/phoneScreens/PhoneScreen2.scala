package phoneScreens

import scala.annotation.tailrec

object PhoneScreen2 {

  /*
   * Asked if I could code it in Java for her.
   *
   * Implement find index of target Integer for a Sorted and Rotated Integer Array
   * without duplicates.
   *
   * Example input:
   * a = [16, 21, 22, 4, 5, 10, 11, 14]
   * t = target integer
   */
  def rotatedFind(t: Int, a: Array[Int]): Option[Int] = {
    rotatedFind(t, a, minIndex = 0, maxIndex = a.size - 1)
  }

  def rotatedFind(t: Int, a: Array[Int], minIndex: Int, maxIndex: Int): Option[Int] = {
//    println(s"min = $minIndex\nmid = ${(minIndex + maxIndex) / 2}\nmax = $maxIndex")
    if (minIndex > maxIndex) {
      None
    } else {
      val midIndex = (minIndex + maxIndex) / 2
      def rightIsSorted = a(midIndex) < a(maxIndex)

      def containsInSortedArray(lowIndex: Int, highIndex: Int): Boolean = {
        lowIndex <= highIndex && a(lowIndex) <= t && t <= a(highIndex)
      }

      if (t == a(midIndex)) {
        Some(midIndex)
      } else if (rightIsSorted) {
        if (t > a(midIndex) && containsInSortedArray(midIndex + 1, maxIndex)) {
          binarySearch(t, a, midIndex + 1, maxIndex)
        } else {
          rotatedFind(t, a, minIndex, midIndex - 1)
        }
      } else {
        if (t < a(midIndex) && containsInSortedArray(minIndex, midIndex - 1)) {
          binarySearch(t, a, minIndex, midIndex - 1)
        } else {
          rotatedFind(t, a, midIndex + 1, maxIndex)
        }
      }
    }
  }

  def binarySearch(t: Int, a: Array[Int]): Option[Int] = {
    binarySearch(t, a, 0, a.size)
  }

  //Basic binary search
  @tailrec
  def binarySearch(t: Int, a: Array[Int], minIndex: Int, maxIndex: Int): Option[Int] = {
    if (minIndex > maxIndex) {
      None
    } else {
      val midIndex = (minIndex + maxIndex) / 2

      if (t == a(midIndex)) {
        Some(midIndex)
      } else if (t < a(midIndex)) {
        binarySearch(t, a, minIndex, midIndex - 1)
      } else {
        binarySearch(t, a, midIndex + 1, maxIndex)
      }
    }
  }
}