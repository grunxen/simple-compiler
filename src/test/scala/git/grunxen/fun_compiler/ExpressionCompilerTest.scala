package git.grunxen.fun_compiler

import org.scalatest.funsuite.AnyFunSuite

class ExpressionCompilerTest extends AnyFunSuite {
  def compileAndExecute(f: FunctionDecl, args: List[Int] = Nil): Option[Any] = {
    val cls = ExpressionCompiler.compile(f)
    ClassExecutor.exec(cls, "fun", args)
  }

  def compileOnlyExpr(e: Expr): Option[Any] = {
    compileAndExecute(FunctionDecl(List(), e))
  }

  test("Simple return expression compiles and executes successfully") {
    val o = compileOnlyExpr(Number(252))
    assert(o.contains(252))
  }

  test("Simple add expression compiles and executes successfully") {
    val o = compileOnlyExpr(Add(Number(25), Number(75)))
    assert(o.contains(100))
  }

  test("Add expression compiles and executes successfully") {
    val o = compileOnlyExpr(Add(Number(25), Add(Number(75), Number(100))))
    assert(o.contains(200))
  }

  test("Function with argument compiles and executes successfully") {
    val o = compileAndExecute(FunctionDecl(List('x'), Argument('x')), List(200))
    assert(o.contains(200))

    val a = compileAndExecute(FunctionDecl(List('x'), Add(Argument('x'), Number(255))), List(200))
    assert(a.contains(455))
  }
}
