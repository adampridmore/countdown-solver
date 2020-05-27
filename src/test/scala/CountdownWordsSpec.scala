package CountdownWordSpec

import org.scalatest.Matchers._
import org.scalatest._

case class CountdownWord(words: Seq[String]){
  
  def search(letters: String) = {

    def isMatch(word: String) = {
      letters == word
    }

    words.find(isMatch)
  }
}

class CountdownWordSpec extends WordSpec {
  "Given a list of words" when {
    "there is not match" in {
      val countdownWord = CountdownWord(Seq("a"))

      val results = countdownWord.search("b")

      results shouldBe None
    }
    
    "there is a match" in {
      val countdownWord = CountdownWord(Seq("a", "b", "c"))

      val results = countdownWord.search("a")

      results shouldBe Some("a")
    }
  }
}