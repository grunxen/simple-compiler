package git.grunxen.fun_compiler

import org.parboiled2._

class FunctionParser(val input: ParserInput) extends Parser {

  def RFunction: Rule1[FunctionDecl] = rule {
    Signature ~ ch('=') ~ LowPrecOperation ~ EOI ~> ((s: List[Char], e: Expr) => FunctionDecl(s, e))
  }

  def Signature: Rule1[List[Char]] = rule {
    str("fun(") ~ optional(Arguments) ~ str(")") ~> ((o: Option[List[Char]]) => o.getOrElse(List()))
  }

  def Arguments: Rule1[List[Char]] = rule {
    zeroOrMore(StartArgs) ~ Arg ~> ((args: Seq[Char], c: Char) => List.from(args :+ c))
  }

  def StartArgs: Rule1[Char] = rule {
    Arg ~ ch(',')
  }

  def Arg: Rule1[Char] = rule {
    capture(CharPredicate.LowerAlpha) ~> ((s: String) => s.charAt(0))
  }

  def Parens: Rule1[Expr] = rule {
    '(' ~ LowPrecOperation ~ ')'
  }

  def LowPrecOperation: Rule1[Expr] = rule {
    HighPrecOperation ~ zeroOrMore(
      '+' ~ HighPrecOperation ~> ((e1: Expr, e2: Expr) => Add(e1, e2))
        | '-' ~ HighPrecOperation ~> ((e1: Expr, e2: Expr) => Sub(e1, e2))
    )
  }

  def HighPrecOperation: Rule1[Expr] = rule {
    Operand ~ zeroOrMore(
      '*' ~ Operand ~> ((e1: Expr, e2: Expr) => Mul(e1, e2))
        | '/' ~ Operand ~> ((e1: Expr, e2: Expr) => Div(e1, e2))
    )
  }


  def Operand: Rule1[Expr] = rule {
    RNumber | RArgument | Parens
  }

  def RArgument: Rule1[Argument] = rule {
    Arg ~> ((c: Char) => Argument(c))
  }

  def RNumber: Rule1[Number] = rule {
    capture(Digits) ~> ((s: String) => Number(s.toInt))
  }

  def Digits = rule {
    oneOrMore(CharPredicate.Digit)
  }
}
