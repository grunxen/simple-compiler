package git.grunxen.fun_compiler

sealed trait Operator {
  val expr1: Expr
  val expr2: Expr
}

sealed trait Expr
case class Number(v: Int) extends Expr
case class Argument(c: Char) extends Expr
case class Add(expr1: Expr, expr2: Expr) extends Expr with Operator
case class Sub(expr1: Expr, expr2: Expr) extends Expr with Operator
case class Mul(expr1: Expr, expr2: Expr) extends Expr with Operator
case class Div(expr1: Expr, expr2: Expr) extends Expr with Operator
