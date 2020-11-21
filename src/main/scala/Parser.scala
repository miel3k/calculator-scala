import scala.util.matching.Regex

object Parser {
  private val numberRegex: Regex = "(\\d+)".r
  private val operatorRegex: Regex = "([+\\-*/()])".r

  def parseTokens(tokens: String): List[Token] = {
    tokens.split("\\s+").toList map parseToken
  }

  def parseToken(token: String): Token = {
    token match {
      case operatorRegex(it) ⇒ Operator(it)
      case numberRegex(it) ⇒ Number(it.toInt)
      case _ ⇒ Invalid
    }
  }
}
