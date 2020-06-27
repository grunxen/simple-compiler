package git.grunxen.fun_compiler

import org.scalatest.funsuite.AnyFunSuite

import scala.util.Success

class ExpressionParserTest extends AnyFunSuite {
  test("Expression parser should properly parse numbers") {
    val parser = new FunctionParser("123")
    assert(parser.RNumber.run() == Success(Number(123)))
  }

  test("Expression parser should properly parse add expression") {
    val parser = new FunctionParser("1+2")
    assert(parser.Operation.run() == Success(Add(Number(1), Number(2))))
  }

  test("Expression parser should properly parse function signature") {
    val parser = new FunctionParser("fun(x,y)")
    assert(parser.Signature.run() == Success(Set('x', 'y')))

    val parser2 = new FunctionParser("fun()")
    assert(parser2.Signature.run() == Success(Set()))
  }

  test("Expression parser should properly parse function declaration") {
    val parser = new FunctionParser("fun(x)=1")
    assert(parser.RFunction.run() == Success(FunctionDecl(Set('x'), Number(1))))
  }
}
