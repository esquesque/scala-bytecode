package scala.bytecode.test.ifs

import scala.bytecode.asm._
import scala.bytecode.ast._

object if_AND_Or_and extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 4
  val name = "if_((_&&_)||_)&&_"
  val desc = "(ZZZZ)V"
  val insns = {
    val lbl0 = label()
    val lbl1 = label()
    val lbl2 = label()
    insnList(
      iload(0),
      ifeq(lbl0),
      iload(1),
      ifne(lbl1),
      lbl0,
      iload(2),
      ifeq(lbl2),
      lbl1,
      iload(3),
      ifeq(lbl2),
      invokestatic("foo", "bar", "()V"),
      lbl2,
      vreturn())
  }

  val test: Test = {
    case Exec(If(And(Or(And(Ne(_, _), Ne(_, _)), Ne(_, _)), Ne(_, _)), _) :: _) => true
    case tree => false
  }
}
