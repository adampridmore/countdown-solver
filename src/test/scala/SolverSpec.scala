package SolverSpec

import CountdownWordSpec._
import CountdownWord._

import org.scalatest.Matchers._
import org.scalatest._

class SolverSpec extends WordSpec {
  "solve" ignore {
   import scala.io.Source

    val lines = Source.fromResource("english3.txt").getLines.toIterable
    
    val countdownWord = CountdownWord(lines)
    println(countdownWord.search("ERCZUREIF"))
  }
}