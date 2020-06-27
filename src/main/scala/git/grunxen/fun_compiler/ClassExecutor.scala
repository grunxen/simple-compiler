package git.grunxen.fun_compiler

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.tree.ClassNode

import scala.util.Try


object ClassExecutor {
  lazy private val loader = new FunctionClassLoader

  def getFunction(c: ClassNode): Either[Throwable, ExpressionFunction] = Try {
    val value = loader.loadGeneratedClass(c)
    val fun = value.getDeclaredMethods.find(_.getName == ExpressionCompiler.methodName).get
    new ExpressionFunction {
      override def apply(args: Int*): Int = fun.invoke(null, args: _*).asInstanceOf[Int]
    }
  }.toEither

  def exec(c: ClassNode, args: Int*): Either[Throwable, Int]  =
    getFunction(c).map(_.apply(args: _*))
}

private class FunctionClassLoader extends ClassLoader {
  def loadGeneratedClass(c: ClassNode): Class[_] = {
    val writer = new ClassWriter(ClassWriter.COMPUTE_FRAMES + ClassWriter.COMPUTE_MAXS)
    c.accept(writer)
    val bytes = writer.toByteArray
    defineClass(null, bytes, 0, bytes.length)
  }
}
