package git.grunxen.fun_compiler

import org.parboiled2._

class ExpressionParser(val input: ParserInput) extends Parser {

  def Function: Rule1[Expr] = rule { Operation ~ EOI }

  def Operation: Rule1[Expr] = rule { RNumber ~ optional(RAdd) ~> ((n: Number, o: Option[Number]) => o match {
    case Some(value) => Add(n, value)
    case None => n
  })}

  def Digits = rule { oneOrMore(CharPredicate.Digit) }
  def RNumber: Rule1[Number] = rule { capture(Digits) ~> ((s: String) => Number(s.toInt)) }

  def RAdd: Rule1[Number] = rule { ch('+') ~ RNumber }
}
