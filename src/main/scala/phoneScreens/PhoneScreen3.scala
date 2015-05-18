package phoneScreens

object PhoneScreen3 {

  /*
   * Split up string into words and make string of them with their frequency in descending
   * frequency order with each entry on a new line with the work followed by a colon and a space.
   * 
   * Sort output in descending frequency order
   *
   * Example:
   *   input: "apple cherry cherry banana apple apple"
   *   output:
   *     apple: 3
   *     cherry: 2
   *     banana: 1
   */
  def countWordFrequencies(input: String): Map[String, Int] = {
    val words = input.split(Array(' ', '\t', '\n')).filter(_.nonEmpty)
    val wordToMatches: Map[String, Array[String]] = words.groupBy(word => word)
    for {
      (word, freq) <- wordToMatches
    } yield {
      word -> freq.size
    }
  }

  def formattedWordFrequencies(input: String): String = {
    val formattedEntries = for {
      (word, freq) <- countWordFrequencies(input).toSeq.sortBy { case (_, freq) => -freq }
    } yield {
      s"$word: $freq"
    }
    formattedEntries.mkString("\n")
  }
}