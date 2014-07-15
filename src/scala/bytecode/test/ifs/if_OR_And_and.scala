package scala.bytecode.test.ifs

import scala.bytecode.asm._
import scala.bytecode.ast._

//same as Or_and_And (and interpreted as such)
//both TT=0000000000000111
object if_OR_And_and extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 4
  val name = "if_((_||_)&&_)&&_"
  val desc = "(ZZZZ)V"
  val insns = {
    val lbl0 = label()
    val lbl1 = label()
    insnList(
      iload(0),
      ifne(lbl0),
      iload(1),
      ifeq(lbl1),
      lbl0,
      iload(2),
      ifeq(lbl1),
      iload(3),
      ifeq(lbl1),
      invokestatic("foo", "bar", "()V"),
      lbl1,
      vreturn())
  }

  val test: Test = {
    case Exec(If(And(Or(True(_), True(_)), And(True(_), True(_))), _) ::
	      Label(_) ::
	      Return(_) ::
	      Nil) => true
    case tree => false
  }
}
