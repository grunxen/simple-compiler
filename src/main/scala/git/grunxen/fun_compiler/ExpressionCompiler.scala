package git.grunxen.fun_compiler

import java.util.UUID

import org.objectweb.asm.Opcodes._
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.tree._

object ExpressionCompiler {

  def compile(expr: FunctionDecl): ClassNode = {
    val cls = prepareClass
    val fun = cls.visitMethod(ACC_PUBLIC + ACC_STATIC, "fun", descriptor(expr.args.length), null, null)
    generateMethod(expr, fun)
    cls
  }

  private def descriptor(count: Int) = "(" + ("I" * count) + ")I"

  private def prepareClass: ClassNode = {
    val cls = new ClassNode()
    cls.name = "git/grunxen/math_compiler/ExpressionEvaluator$" + UUID.randomUUID().toString.replace("-", "")
    cls.superName = "java/lang/Object"
    cls.access = ACC_PUBLIC + ACC_FINAL
    cls.version = V1_8
    cls
  }

  private def generateMethod(expr: FunctionDecl, m: MethodVisitor): MethodVisitor = {
    val argsIndx = expr.args.zipWithIndex.toMap
    visitExpr(expr.expr, m, argsIndx)
    m.visitInsn(IRETURN)
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
