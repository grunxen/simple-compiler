package git.grunxen.fun_compiler

object Main {
  def main(args: Array[String]): Unit = {
    val cls = ExpressionCompiler.compile(Number(12345))
    println(ClassExecutor.exec(cls, "fun", Nil))
  }
}
