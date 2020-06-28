package git.grunxen.fun_compiler

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.tree.ClassNode

import scala.util.Try


object ClassExecutor {
  lazy private val loader = new FunctionClassLoader

  def getFunction(c: ClassNode): Either[Throwable, ExpressionFunction] = Try {
    val value = loader.loadGeneratedClass(c)
    value.getDeclaredConstructor().newInstance()
  }.toEither

  def exec(c: ClassNode, args: Int*): Either[Throwable, Int]  =
    getFunction(c).map(_.apply(args: _*))
}

private class FunctionClassLoader extends ClassLoader {
  def loadGeneratedClass(c: ClassNode): Class[_ <: ExpressionFunction] = {
    val writer = new ClassWriter(ClassWriter.COMPUTE_FRAMES + ClassWriter.COMPUTE_MAXS)
    c.accept(writer)
    val bytes = writer.toByteArray
    val clazz = defineClass(null, bytes, 0, bytes.length)
    clazz.asInstanceOf[Class[_ <: ExpressionFunction]]
  }
}
