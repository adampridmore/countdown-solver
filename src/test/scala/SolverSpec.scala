package SolverSpec

import CountdownWordSpec._
import CountdownWord._

import org.scalatest.Matchers._
import org.scalatest._
import scala.io.Source

class SolverSpec extends WordSpec {

  "solve" in {
    val countdownWord = CountdownWord.defaultDictionary

    val solutions = countdownWord.search("ERCZUREIF")

    solutions.words shouldBe(List("fiercer", "furzier"))
    println(solutions)
  }

  "solve2" in {    
    val countdownWord = CountdownWord.defaultDictionary

    val solutions = countdownWord.search("DEADBONER")
    
    println(solutions.formatted())
  }
}
