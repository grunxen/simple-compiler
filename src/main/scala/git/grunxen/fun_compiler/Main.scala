package git.grunxen.fun_compiler

object Main {
  def main(args: Array[String]): Unit = {
    val cls = ExpressionCompiler.compile(FunctionDecl(List(), Add(Number(25), Number(50))))
    println(ClassExecutor.exec(cls, "fun", Nil))
  }
}
