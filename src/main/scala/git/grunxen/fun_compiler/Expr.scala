package git.grunxen.fun_compiler

sealed trait Expr
case class Number(v: Int) extends Expr
case class Argument(c: Char) extends Expr
case class Add(expr1: Expr, expr2: Expr) extends Expr
