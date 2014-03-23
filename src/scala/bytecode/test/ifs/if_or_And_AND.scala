package scala.bytecode.test.ifs

import scala.bytecode.asm._
import scala.bytecode.ast._

//same as or_And_and
//both TT=0101010101010111
object if_or_And_AND extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 4
  val name = "if__||(_&&(_&&_))"
  val desc = "(ZZZZ)V"
  val insns = {
    val lbl0 = label()
    val lbl1 = label()
    insnList(
      iload(0),
      ifne(lbl0),
      iload(1),
      ifeq(lbl1),
      iload(2),
      ifeq(lbl1),
      iload(3),
      ifeq(lbl1),
      lbl0,
      invokestatic("foo", "bar", "()V"),
      lbl1,
      vreturn())
  }

  val test: Test = {
    case Exec(If(Or(True(_), And(True(_), And(True(_), True(_)))), _) :: _) => true
    case tree => false
  }
}
