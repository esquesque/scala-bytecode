package scala.bytecode.test.ifs

import scala.bytecode.asm._
import scala.bytecode.ast._

object if_else extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 1
  val name = "if_else"
  val desc = "(Z)V"
  val insns = {
    val lbl0 = label()
    val lbl1 = label()
    insnList(
      iload(0),
      ifeq(lbl0),
      invokestatic("foo", "bar", "()V"),
      goto(lbl1),
      lbl0,
      invokestatic("foo", "baz", "()V"),
      lbl1,
      vreturn())
  }

  val test: Test = {
    case Exec(If(True(_), Then(_), Else(_)) :: _) => true
    case tree => false
  }
}
