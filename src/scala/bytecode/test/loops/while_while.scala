package scala.bytecode.test.loops

import scala.bytecode.asm._
import scala.bytecode.ast._

object while_while extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 2
  val name = "while_while"
  val desc = "(ZZ)V"
  val insns = {
    val (lbl0, lbl1, lbl2, lbl3) = (label(), label(), label(), label())
    insnList(
      lbl0,
      iload(0),
      ifeq(lbl3),
      lbl1,
      iload(1),
      ifeq(lbl2),
      invokestatic("foo", "bar", "()V"),
      goto(lbl1),
      lbl2,
      goto(lbl0),
      lbl3,
      vreturn())
  }

  val test: Test = {
    case Exec(If(True(_), Then(_)) :: Label(_) :: Return(_) :: Nil) => true
    case tree => false
  }
}
