package scala.bytecode.test.ternaries

import scala.bytecode.asm._
import scala.bytecode.ast._

object zero_stack extends scala.bytecode.test.ASTCase {
  val maxStack = 3
  val maxLocals = 2
  val name = "z_stack"
  val desc = "(Z)V"
  val insns = {
    val lbl0 = label()
    val lbl1 = label()
    insnList(
      iload(0),
      ifeq(lbl0),
      apush("x"),
      goto(lbl1),
      lbl0,
      apush("y"),
      lbl1,
      astore(1),
      vreturn())
  }

  val test: Test = {
    case Exec(If(Ne(_, _), Then(_), Else(_)) ::
	      Label(_) ::
	      LocalStore(_, _) :: _) => true
    case _ => false
  }
}
