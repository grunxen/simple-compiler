package git.grunxen.fun_compiler

import org.parboiled2._

class ExpressionParser(val input: ParserInput) extends Parser {

  def RNumber: Rule1[Number] = rule { capture(Digits) ~> ((s: String) => Number(s.toInt)) }

  def Digits = rule { oneOrMore(CharPredicate.Digit) }
}
