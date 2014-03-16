package scala.bytecode.test.ifs

import scala.bytecode.asm._
import scala.bytecode.ast._

object if_and_and_and extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 4
  val name = "if__&&_&&_&&_"
  val desc = "(ZZZZ)V"
  val insns = {
    val lbl = label()
    insnList(
      iload(0),
      ifeq(lbl),
      iload(1),
      ifeq(lbl),
      iload(2),
      ifeq(lbl),
      iload(3),
      ifeq(lbl),
      invokestatic("foo", "bar", "()V"),
      lbl,
      vreturn())
  }

  val test: Test = {
    case Exec(If(And(True(_), And(True(_), And(True(_), True(_)))), _) :: _) => true
    case tree => false
  }
}
