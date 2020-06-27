package git.grunxen.fun_compiler

import org.scalatest.funsuite.AnyFunSuite

class ExpressionCompilerTest extends AnyFunSuite {
  def compileAndExecute(expr: Expr): Option[Any] = {
    val cls = ExpressionCompiler.compile(expr)
    ClassExecutor.exec(cls, "fun", Nil)
  }

  test("Simple return expression compiles and executes successfully") {
    val o = compileAndExecute(Number(252))
    assert(o.contains(252))
  }

  test("Simple add expression compiles and executes successfully") {
    val o = compileAndExecute(Add(Number(25), Number(75)))
    assert(o.contains(100))
  }

  test("Add expression compiles and executes successfully") {
    val o = compileAndExecute(Add(Number(25), Add(Number(75), Number(100))))
    assert(o.contains(200))
  }
}
