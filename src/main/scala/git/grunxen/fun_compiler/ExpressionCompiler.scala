package git.grunxen.fun_compiler

import org.objectweb.asm.Opcodes._
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.tree._

object ExpressionCompiler {

  def compile(expr: Expr): ClassNode = {
    val cls = prepareClass
    val fun = cls.visitMethod(ACC_PUBLIC + ACC_STATIC, "fun", "()I", null, null)
    generateMethod(expr, fun)
    cls
  }

  private def prepareClass: ClassNode = {
    val cls = new ClassNode()
    cls.name = "git/grunxen/math_compiler/ExpressionEvaluator$$$"
    cls.superName = "java/lang/Object"
    cls.access = ACC_PUBLIC + ACC_FINAL
    cls.version = V1_8
    cls
  }

  private def generateMethod(expr: Expr, m: MethodVisitor): MethodVisitor = {
    expr match {
      case Number(i) =>
        m.visitLdcInsn(i)
        m.visitInsn(IRETURN)
    }
    m
  }
}
