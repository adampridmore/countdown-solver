package CountdownWord

case class Word(text: String) {
  lazy val length = text.length

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

    f(letters.toLowerCase.sortBy(identity), text.sortBy(identity))
  }
}

case class CountdownWord(words: Seq[String]){

  val words2 = words.map(Word.apply)

  def search(letters: String) : Seq[String] = {
    val solutions = words2
      .filter(_.canBeMadeFrom(letters))
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
