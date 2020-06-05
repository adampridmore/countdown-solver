package CountdownWord

case class Word(text: String) {
  lazy val length = text.length
}

case class CountdownWord(words: Iterable[String]){

  val words2 = words.map(Word.apply)

  def search(letters: String) : Iterable[String] = {
    val solutions = words2
      .filter(word => CountdownWord.containsWord(letters.toLowerCase(), word.text))
      .map(_.text)

    if (solutions.nonEmpty){
      (solutions
        .groupBy(w=>w.length)
        .maxBy{case (length, _) => length})._2
    } else {
      Seq.empty
    }    
  }
}

object CountdownWord {
  def containsWord(letters: String, word: String) : Boolean = {

    def f(letters: String, word: String) : Boolean = {

      (letters.headOption, word.headOption) match {
        case (_, None) => true
        case (None, _) => false
        case (Some(l), Some(w)) if (l == w) => containsWord(letters.tail, word.tail) 
        case (Some(l), Some(w)) => containsWord(letters.tail, word)
        case _ => false
      }
    }

    f(letters.sortBy(identity), word.sortBy(identity))
  }
}
