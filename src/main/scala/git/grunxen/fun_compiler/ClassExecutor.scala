package git.grunxen.fun_compiler

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.tree.ClassNode


object ClassExecutor {
  lazy private val loader = new ExecutionClassLoader

  def exec(c: ClassNode, methodName: String, args: List[Any]): Option[Any] = {
    val cls = loader.loadGeneratedClass(c)
    cls.getDeclaredMethods.find(f => f.getName.equals(methodName)).map(m => m.invoke(null, args:_*))
  }
}

class ExecutionClassLoader extends ClassLoader {
  def loadGeneratedClass(c: ClassNode): Class[_] = {
    val writer = new ClassWriter(ClassWriter.COMPUTE_FRAMES + ClassWriter.COMPUTE_MAXS)
    c.accept(writer)
    val bytes = writer.toByteArray
    defineClass(null, bytes, 0, bytes.length)
  }
}
