sealed abstract class Token

case class Number(value: Int) extends Token

case class Operator(name: String) extends Token

case object Invalid extends Token
