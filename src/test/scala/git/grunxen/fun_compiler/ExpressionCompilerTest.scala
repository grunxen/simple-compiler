package git.grunxen.fun_compiler

import org.scalatest.funsuite.AnyFunSuite


class ExpressionCompilerTest extends AnyFunSuite {
  def compileAndExecute(f: FunctionDecl, args: Int*): Either[Throwable, Int] = {
    val cls = ExpressionCompiler.compile(f)
    ClassExecutor.exec(cls.get, args: _*)
  }

  def compileOnlyExpr(e: Expr): Either[Throwable, Int] = {
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

  test("Simple function with argument compiles and executes successfully") {
    val o = compileAndExecute(FunctionDecl(List('x'), Argument('x')), 200)
    assert(o.contains(200))

    val a = compileAndExecute(FunctionDecl(List('x'), Add(Argument('x'), Number(255))), 200)
    assert(a.contains(455))
  }

  test("Function with argument compiles and executes successfully") {
    // (x+1)*2+y
    val add = Add(Mul(Add(Argument('x'), Number(1)), Number(2)), Argument('y'))
    val o = compileAndExecute(FunctionDecl(List('x', 'y'), add), 5, 8)
    assert(o.contains(20))
  }

  test("Function compilation fails if argument not defined") {
    val f = ExpressionCompiler.compile(FunctionDecl(List(), Argument('x')))
    assert(f.isFailure)
  }
}
