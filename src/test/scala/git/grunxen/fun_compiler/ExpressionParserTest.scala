package git.grunxen.fun_compiler

import org.scalatest.funsuite.AnyFunSuite

import scala.util.Success

class ExpressionParserTest extends AnyFunSuite {
  test("Expression parser should properly parse numbers") {
    val parser = new ExpressionParser("123")
    assert(parser.RNumber.run() == Success(Number(123)))
  }
}
