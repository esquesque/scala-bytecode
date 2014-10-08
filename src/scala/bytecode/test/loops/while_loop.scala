package scala.bytecode.test.loops

import scala.bytecode.asm._
import scala.bytecode.ast._

object while_loop extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 1
  val name = "while"
  val desc = "(Z)V"
  val insns = {
    val lbl0 = label()
    val lbl1 = label()
    insnList(
      lbl0,
      iload(0),
      ifeq(lbl1),
      invokestatic("foo", "bar", "()V"),
      goto(lbl0),
      lbl1,
      vreturn())
  }

  val test: Test = {
    case Exec(If(True(_), Then(_)) :: Label(_) :: Return(_) :: Nil) => true
    case tree => false
  }
}
