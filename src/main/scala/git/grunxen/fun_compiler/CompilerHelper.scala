package git.grunxen.fun_compiler

object CompilerHelper {
  implicit class ClassUtils[T](clazz: Class[T]) {
    def jvmName: String = clazz.getCanonicalName.replace('.', '/')
  }
}
