package chapter1

object Chapter1Solution {
  /**
   * Sort and then check there are no duplicates next to each other
   * O(n log n)
   *
   * Without limiting the size of the character input set, I don't think you can have an O(n) answer
   * without using a collection like a HashSet
   */
  def stringHasAllUniqueCharacters(str: String): Boolean = {
    def hasDuplicates(str: String): Boolean = (str.size == 2) && (str.charAt(0) == str.charAt(1))
    str.sorted.sliding(2).forall { subStr => !hasDuplicates(subStr) }
  }

  def arePermutations(s1: String, s2: String): Boolean = {
//    s1.permutations.contains(s2)
    s1.sorted == s2.sorted
  }

  def replaceSpaces(chars: Array[Char], length: Int): Unit = {
    var numSpaces = 0
    for {
      i <- 0 until length
      if chars(i).isSpaceChar
    } {
      numSpaces += 1
    }

    var index = length - 1 + 2*numSpaces // already included ' ', so needs to be 2 chars longer for each space in result
    for {
      i <- length - 1 to 0 by -1
      next = chars(i)
    } {
      if (next.isSpaceChar) {
        chars(index - 0) = '0'
        chars(index - 1) = '2'
        chars(index - 2) = '%'
        index -= 3
      } else {
        chars(index) = next
        index -= 1
      }
    }
  }

  def compressString(s: String): String = {
    if (s.size < 2) {
      s
    } else {
      var numSame = 1
      val codedChars = for {
        subStr <- s.sliding(2)
      } yield {
        val r = subStr match {
          case s =>
            val c1 = subStr.charAt(0)
            val c2 = subStr.charAt(1)
            if (c1 != c2) {
              val result = s"$c1$numSame"
              numSame = 1
              result
            } else {
              numSame += 1
              ""
            }
        }
        r
      }

      val firstPart = codedChars.mkString
      val lastPart = s"${s.last}$numSame"

      val result = firstPart + lastPart

      //return smaller
      if (s.size < result.size) {
        s
      } else {
        result
      }
    }
  }
}