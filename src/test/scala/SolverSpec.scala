package SolverSpec

import CountdownWordSpec._
import CountdownWord._

import org.scalatest.Matchers._
import org.scalatest._

class SolverSpec extends WordSpec {
  "solve" in {
   import scala.io.Source

    val lines = Source.fromResource("english3.txt").getLines.toIterable
    
    val countdownWord = CountdownWord(lines.toSeq)

    val solutions = countdownWord.search("ERCZUREIF")

    solutions shouldBe(List("fiercer", "furzier"))
    println(countdownWord.search("ERCZUREIF"))
  }
}
