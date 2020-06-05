package CountdownWord

case class Word(text: String) {
  val length = text.length
  val sortedLetters = text.sortBy(identity)

  def canBeMadeFrom(letters: String) = {
     def f(letters: String, word: String) : Boolean = {

      (letters.headOption, word.headOption) match {
        case (_, None) => true
        case (None, _) => false
        case (Some(l), Some(w)) if (l == w) => f(letters.tail, word.tail) 
        case (Some(l), Some(w)) => f(letters.tail, word)
        case _ => false
      }
    }

    f(letters.toLowerCase.sortBy(identity), sortedLetters)
  }
}


case class Solution(words: Seq[String]) {
  def formatted() = {
    val text = words
      .map(word=>s"$word (${word.length})")
      .mkString("\n")
    text
  }
}

object Solution {
  val empty = Solution(Seq.empty)
}

case class CountdownWord(words: Seq[String]){

  private val words2 = words.map(Word.apply)

  def search(letters: String) : Solution = {
    val solutions = words2
      .filter(_.canBeMadeFrom(letters))
      .map(_.text)

    if (solutions.nonEmpty) {
      Solution((solutions
        .groupBy(w=>w.length)
        .maxBy{case (length, _) => length})._2)
    } else {
      Solution.empty
    }
  }
}

object CountdownWord {

  import scala.io.Source

  val lines = Source.fromResource("english3.txt").getLines.toIterable

  lazy val defaultDictionary = CountdownWord(lines.toSeq)
}
