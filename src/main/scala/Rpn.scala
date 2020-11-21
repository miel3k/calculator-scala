import scala.collection.mutable

class Rpn(tokens: List[Token]) {

  def calculate(): Int = {
    val output = convertToPostfix().reverse
    val stack: mutable.Stack[Int] = mutable.Stack.empty
    output.foreach {
      case operator@Operator(_) =>
        val x = stack.pop()
        val y = stack.pop()
        stack.push(calculate(operator, x, y))
      case Number(t) => stack.push(t)
    }
    stack.pop()
  }

  private def convertToPostfix(): mutable.Stack[Token] = {
    val operators: mutable.Stack[Operator] = mutable.Stack.empty
    val output: mutable.Stack[Token] = mutable.Stack.empty
    tokens.foreach {
      case token@Operator("(") => operators.push(token)
      case Operator(")") =>
        var isOpeningBracket = false
        while (!isOpeningBracket && operators.nonEmpty) {
          val stackValue = operators.pop()
          if (stackValue.name == "(") {
            isOpeningBracket = true
          } else {
            output.push(stackValue)
          }
        }
      case token@Operator(name) =>
        if (operators.isEmpty) {
          operators.push(Operator(name))
        } else {
          if (getPriority(token) <= getPriority(operators.top)) {
            var isPriorityHigher = true
            while (isPriorityHigher && operators.nonEmpty) {
              if (getPriority(operators.top) >= getPriority(token)) {
                output.push(operators.pop())
              } else {
                isPriorityHigher = false
              }
            }
          }
          operators.push(token)
        }
      case token => output.push(token)
    }
    while (operators.nonEmpty) {
      output.push(operators.pop())
    }
    output
  }

  private def getPriority(operator: Operator): Int = {
    operator.name match {
      case "+" | "-" => 1
      case "*" | "/" => 2
      case "(" | ")" => 0
      case _ => 0
    }
  }

  private def calculate(operator: Operator, x: Int, y: Int): Int = {
    operator match {
      case Operator("+") => x + y
      case Operator("-") => y - x
      case Operator("*") => x * y
      case Operator("/") => y / x
    }
  }
}
