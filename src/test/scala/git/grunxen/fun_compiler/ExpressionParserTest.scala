package git.grunxen.fun_compiler

import org.scalatest.funsuite.AnyFunSuite

import scala.util.Success

class ExpressionParserTest extends AnyFunSuite {
  test("Expression parser should properly parse numbers") {
    val parser = new FunctionParser("123")
    assert(parser.Operand.run() == Success(Number(123)))
  }

  test("Expression parser should properly argument") {
    val parser = new FunctionParser("x")
    assert(parser.Operand.run() == Success(Argument('x')))
  }

  test("Expression parser should properly parse simple arithmetic expressions") {
    assert(new FunctionParser("1+x").LowPrecOperation.run() == Success(Add(Number(1), Argument('x'))))
    assert(new FunctionParser("1-x").LowPrecOperation.run() == Success(Sub(Number(1), Argument('x'))))
    assert(new FunctionParser("1*x").LowPrecOperation.run() == Success(Mul(Number(1), Argument('x'))))
    assert(new FunctionParser("1/x").LowPrecOperation.run() == Success(Div(Number(1), Argument('x'))))
  }

  test("Expression parser should properly parse function signature") {
    val parser = new FunctionParser("fun(x,y)")
    assert(parser.Signature.run() == Success(List('x', 'y')))

    val parser2 = new FunctionParser("fun()")
    assert(parser2.Signature.run() == Success(List()))
  }

  test("Expression parser should properly parse function declaration") {
    val parser = new FunctionParser("fun(x)=1")
    assert(parser.RFunction.run() == Success(FunctionDecl(List('x'), Number(1))))
  }

  test("Expression parser should properly parse complex function declaration") {
    val parser = new FunctionParser("fun(x,y)=(x+1)*2+y")
    assert(parser.RFunction.run() == Success(FunctionDecl(List('x', 'y'), Add(Mul(Add(Argument('x'), Number(1)), Number(2)), Argument('y')))))
  }
}
