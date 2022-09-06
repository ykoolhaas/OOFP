
import collection.mutable.Stack

object ReversePolish {


  def isNumber(s: String): Boolean = {
    if(s.isEmpty) return false
    for(c <- s.toCharArray) {
      if(!c.isDigit) return false
    }
    return true
  }

  def calculate(expression : String) : Int = {
    val s = new Stack[Int]()
    for(el <- expression.split(" ")) {
      if(el == "+" || el == "*" || el == "/") {
        val rhs = s.pop()
        val lhs = s.pop()
        val res =
          if(el == "+") lhs + rhs
          else if(el == "*") lhs * rhs
          else lhs / rhs
        s.push(res)
      } else if(isNumber(el)) s.push(el.toInt)
      else throw new Error("Unknown expression element " + el)
    }
    s.pop()
  }


}
