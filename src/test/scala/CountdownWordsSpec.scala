package CountdownWordSpec

import org.scalatest.Matchers._
import org.scalatest._
import CountdownWord._

class CountdownWordSpec extends WordSpec {
  "isMatch" when {
    "abc does not containsWord def" in {
      Word("def").canBeMadeFrom("abc") shouldBe false
    }

    "a containsWord a" in {
      Word("ab").canBeMadeFrom("ab") shouldBe true
    }

    "ab containsWord ab" in {
      Word("ab").canBeMadeFrom("ab") shouldBe true
    }

    "dc containsWord cd" in {
      Word("cd").canBeMadeFrom("dc") shouldBe true
    }

    "abcdef containsWord af" in {
      Word("af").canBeMadeFrom("abcdef") shouldBe true
    }

    "appletreezzzz containsWord apple" in {
      Word("apple").canBeMadeFrom("appletreezzzz") shouldBe true
    }
  }

  "Given a list of single letter words" when {
      val countdownWord = CountdownWord(Seq("a", "b", "c"))

    "there is not a match" in {
      countdownWord.search("z") shouldBe Solution.empty
    }
    
    "there is a match" in {
      countdownWord.search("a") shouldBe Solution(Seq("a"))
    }
  }

  "Given a list of multi letter words" when {
      val countdownWord = CountdownWord(Seq("ab", "bc", "cd", "dc"))

    "there is not a match" in {
      countdownWord.search("z") shouldBe Solution.empty
    }
    
    "there is a match" in {
      countdownWord.search("ba") shouldBe Solution(Seq("ab"))
    }

    "there is multiple matches match" in {
      countdownWord.search("cd") shouldBe Solution(Seq("cd", "dc"))
    }
  }

  "Given a list of multi letter words" when {
    val countdownWord = CountdownWord(Seq("ab"))
     "there is match on some letters" in {
       countdownWord.search("abc") shouldBe Solution(Seq("ab"))
    }
  }

  "Given a list of real words" when {
    "there is a few matches but one is longer" in {
      val countdownWord = CountdownWord(Seq("apple", "appletree", "car"))
      countdownWord.search("appletreezzzz") shouldBe Solution(Seq("appletree"))
    }

    "there is a few matches the same size" in {
      val countdownWord = CountdownWord(Seq("apple", "appel"))
      countdownWord.search("appletreezzzz") shouldBe Solution(Seq("apple", "appel"))
    }
  }
}
