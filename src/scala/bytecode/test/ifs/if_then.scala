package scala.bytecode.test.ifs

import scala.bytecode.asm._
import scala.bytecode.ast._

object if_then extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 2
  val name = "if"
  val desc = "(Z)V"
  val insns = {
    val lbl = label()
    insnList(
      iload(0),
      ifeq(lbl),
      invokestatic("foo", "bar", "()V"),
      lbl,
      vreturn())
  }

  val test: Test = {
    case Exec(If(Ne(_, _), Then(_)) :: _) => true
    case tree => false
  }
}
