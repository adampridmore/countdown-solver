import scala.io.Source
import CountdownWord._

object Main extends App {
  def solve(letters: String) = {
    val countdownWord = CountdownWord.defaultDictionary

    val solutions = countdownWord.search(letters)
    
    println(solutions.formatted())
  }

  if (args.length == 1) {
    val letters = args(0)
    solve(letters)
  } else {
    println("<letters>")
  }
}
