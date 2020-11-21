import scala.io.StdIn

object Calculator {

  def main(args: Array[String]): Unit = run()

  @scala.annotation.tailrec
  private[this] def run(): Unit = {
    println("Type expression or ( Q || q ) to quit")
    println("Example: 2 + ( 2 * 2 )")
    val input = StdIn.readLine()
    input match {
      case "Q" | "q" => println("Quit");
      case _ =>
        val tokens = Parser.parseTokens(input)
        if (tokens.contains(Invalid)) {
          println("Error: Invalid Token")
        } else {
          try {
            println("= " + new Rpn(tokens).calculate())
          } catch {
            case _: Exception => println("Error: Invalid expression")
          }
        }
        run()
    }
  }
}
