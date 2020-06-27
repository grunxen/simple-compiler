package git.grunxen.fun_compiler

import java.util.UUID

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
    cls.name = "git/grunxen/math_compiler/ExpressionEvaluator$" + UUID.randomUUID().toString.replace("-", "")
    cls.superName = "java/lang/Object"
    cls.access = ACC_PUBLIC + ACC_FINAL
    cls.version = V1_8
    cls
  }

  private def generateMethod(expr: Expr, m: MethodVisitor): MethodVisitor = {
    visitExpr(expr, m)
    m.visitInsn(IRETURN)
    m
  }

  private def visitExpr(expr: Expr, m: MethodVisitor) {
    expr match {
      case Number(i) =>
        m.visitLdcInsn(i)
      case Add(expr1, expr2) =>
        visitExpr(expr1, m)
        visitExpr(expr2, m)
        m.visitInsn(IADD)
    }
  }
}
