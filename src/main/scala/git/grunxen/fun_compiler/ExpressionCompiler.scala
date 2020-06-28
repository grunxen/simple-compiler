package git.grunxen.fun_compiler

import java.util.UUID

import org.objectweb.asm.Opcodes._
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.tree._

import scala.util.Try

object ExpressionCompiler {

  val methodName = "apply"

  def compile(expr: FunctionDecl): Try[ClassNode] = {
    Try {
      val cls = prepareClass
      val fun = cls.visitMethod(ACC_PUBLIC + ACC_STATIC, methodName, descriptor(expr.args.length), null, null)
      generateMethod(expr, fun)
      cls
    }
  }

  private def descriptor(count: Int) = "(" + ("I" * count) + ")I"

  private def prepareClass: ClassNode = {
    val cls = new ClassNode()
    cls.name = "git/grunxen/math_compiler/ExpressionFunction$" + UUID.randomUUID().toString.replace("-", "")
    cls.superName = "java/lang/Object"
    cls.access = ACC_PUBLIC + ACC_FINAL
    cls.version = V1_8
    defaultConstructor(cls)
    cls
  }

  private def defaultConstructor(cls: ClassNode): MethodVisitor = {
    val method = cls.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
    method.visitCode()
    method.visitVarInsn(ALOAD, 0)
    method.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
    method.visitInsn(RETURN)
    method.visitMaxs(1, 1)
    method
  }

  private def generateMethod(expr: FunctionDecl, m: MethodVisitor): MethodVisitor = {
    val argsIndx = expr.args.zipWithIndex.toMap
    m.visitCode()
    visitExpr(expr.expr, m, argsIndx)
    m.visitInsn(IRETURN)
    m.visitEnd()
    m
  }

  private def visitExpr(expr: Expr, m: MethodVisitor, argsIndx: Map[Char, Int]) {
    expr match {
      case Number(i) =>
        m.visitLdcInsn(i)
      case Argument(c) =>
        m.visitVarInsn(ILOAD, argsIndx(c))
      case op: Operator =>
        visitExpr(op.expr1, m, argsIndx)
        visitExpr(op.expr2, m, argsIndx)
        val instr = op match {
          case _: Add => IADD
          case _: Sub => ISUB
          case _: Mul => IMUL
          case _: Div => IDIV
        }
        m.visitInsn(instr)
    }
  }
}
