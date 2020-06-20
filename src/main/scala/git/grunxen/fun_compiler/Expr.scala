package git.grunxen.fun_compiler

sealed trait Expr
case class Number(v: Int) extends Expr
