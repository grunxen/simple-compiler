package git.grunxen.fun_compiler

import org.parboiled2._

class FunctionParser(val input: ParserInput) extends Parser {

  def RFunction: Rule1[FunctionDecl] = rule { Signature ~ ch('=') ~ Operation ~ EOI ~> ((s: Set[Char], e: Expr) => FunctionDecl(s, e))}

  def Signature: Rule1[Set[Char]] = rule { str("fun(") ~ optional(Arguments) ~ str(")") ~> ((o: Option[Set[Char]]) => o.getOrElse(Set())) }

  def Arguments: Rule1[Set[Char]] = rule { zeroOrMore(StartArgs) ~ Arg ~> ((args: Seq[Char], c: Char) => Set.from(args :+ c)) }
  def StartArgs: Rule1[Char] = rule { Arg ~ ch(',') }
  def Arg: Rule1[Char] = rule { capture(CharPredicate.LowerAlpha) ~> ((s: String) => s.charAt(0)) }

  def Operation: Rule1[Expr] = rule { Operand ~ optional(RAdd) ~> ((n: Expr, o: Option[Expr]) => o match {
    case Some(value) => Add(n, value)
    case None => n
  })}

  def Digits = rule { oneOrMore(CharPredicate.Digit) }

  def Operand: Rule1[Expr] = rule { RNumber | RArgument }

  def RArgument: Rule1[Argument] = rule { Arg ~> ((c: Char) => Argument(c)) }
  def RNumber: Rule1[Number] = rule { capture(Digits) ~> ((s: String) => Number(s.toInt)) }

  def RAdd: Rule1[Expr] = rule { ch('+') ~ Operand }
}
