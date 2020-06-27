package git.grunxen.fun_compiler

import org.scalatest.funsuite.AnyFunSuite

import scala.util.Success

class ExpressionParserTest extends AnyFunSuite {
  test("Expression parser should properly parse numbers") {
    val parser = new ExpressionParser("123")
    assert(parser.Function.run() == Success(Number(123)))
  }

  test("Expression parser should properly parse expression") {
    val parser = new ExpressionParser("1+2")
    assert(parser.Function.run() == Success(Add(Number(1), Number(2))))
  }
}
