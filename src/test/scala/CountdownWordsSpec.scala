package CountdownWordSpec

import org.scalatest.Matchers._
import org.scalatest._

case class CountdownWord(words: Iterable[String]){
  def search(letters: String) = {
    val solutions = words.filter(word => CountdownWord.containsWord(letters.toLowerCase(), word))

    if (solutions.nonEmpty){
      (solutions
        .groupBy(w=>w.length())
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

class CountdownWordSpec extends WordSpec {
  "isMatch" when {
    "abc does not containsWord def" in {
      CountdownWord.containsWord("abc", "def") shouldBe false
    }

    "a containsWord a" in {
      CountdownWord.containsWord("ab", "ab") shouldBe true
    }

    "ab containsWord ab" in {
      CountdownWord.containsWord("ab", "ab") shouldBe true
    }

    "dc containsWord cd" in {
      CountdownWord.containsWord("dc", "cd") shouldBe true
    }

    "abcdef containsWord af" in {
      CountdownWord.containsWord("abcdef", "af") shouldBe true
    }

    "appletreezzzz containsWord apple" in {
      CountdownWord.containsWord("appletreezzzz", "apple") shouldBe true
    }
  }

  "Given a list of single letter words" when {
      val countdownWord = CountdownWord(Seq("a", "b", "c"))

    "there is not a match" in {
      countdownWord.search("z") shouldBe Seq.empty
    }
    
    "there is a match" in {
      countdownWord.search("a") shouldBe Seq("a")
    }
  }

  "Given a list of multi letter words" when {
      val countdownWord = CountdownWord(Seq("ab", "bc", "cd", "dc"))

    "there is not a match" in {
      countdownWord.search("z") shouldBe Seq.empty
    }
    
    "there is a match" in {
      countdownWord.search("ba") shouldBe Seq("ab")
    }

    "there is multiple matches match" in {
      countdownWord.search("cd") shouldBe Seq("cd", "dc")
    }
  }

  "Given a list of multi letter words" when {
    val countdownWord = CountdownWord(Seq("ab"))
     "there is match on some letters" in {
       countdownWord.search("abc") shouldBe Seq("ab")
    }
  }

  "Given a list of real words" when {
    "there is a few matches but one is longer" in {
      val countdownWord = CountdownWord(Seq("apple", "appletree", "car"))
      countdownWord.search("appletreezzzz") shouldBe Seq("appletree")
    }

    "there is a few matches the same size" in {
      val countdownWord = CountdownWord(Seq("apple", "appel"))
      countdownWord.search("appletreezzzz") shouldBe Seq("apple", "appel")
    }
  }
}