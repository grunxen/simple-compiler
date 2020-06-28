package git.grunxen.fun_compiler

import java.util.UUID

import org.objectweb.asm.Opcodes._
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.tree._

import scala.util.Try

object ExpressionCompiler {

  private val descriptor = "(Lscala/collection/immutable/Seq;)I"

  def compile(expr: FunctionDecl): Try[ClassNode] = {
    Try {
      val cls = prepareClass
      val fun = cls.visitMethod(ACC_PUBLIC, "apply", descriptor, null, null)
      generateMethod(expr, fun)
      cls
    }
  }


  private def prepareClass: ClassNode = {
    import scala.jdk.CollectionConverters._
    import CompilerHelper._

    val cls = new ClassNode()
    cls.name = "git/grunxen/math_compiler/ExpressionFunctionImpl$" + UUID.randomUUID().toString.replace("-", "")
    cls.superName = "java/lang/Object"
    cls.access = ACC_PUBLIC + ACC_FINAL
    cls.version = V1_8
    cls.interfaces = List(classOf[ExpressionFunction].jvmName).asJava
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
        // java uses arrays for varargs implementation, scala uses Seq[T]

        // load Seq[Int] on the stack
        // this has index 0, Seq[Int] has index 1
        m.visitVarInsn(ALOAD, 1)
        // load index of variable
        m.visitLdcInsn(argsIndx(c))
        // invoke apply method
        m.visitMethodInsn(INVOKEINTERFACE, "scala/collection/immutable/Seq", "apply", "(I)Ljava/lang/Object;", true)
        // unbox variable
        m.visitMethodInsn(INVOKESTATIC, "scala/runtime/BoxesRunTime", "unboxToInt", "(Ljava/lang/Object;)I", false)
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
