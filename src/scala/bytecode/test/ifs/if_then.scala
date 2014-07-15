package scala.bytecode.test.ifs

import scala.bytecode.asm._
import scala.bytecode.ast._

object if_then extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 1
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
    case Exec(If(True(_), Then(_)) :: Label(_) :: Return(_) :: Nil) => true
    case tree => false
  }
}
