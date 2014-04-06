package scala.bytecode.test.ifs

import scala.bytecode.asm._
import scala.bytecode.ast._

object if_elif_else extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 2
  val name = "if_elif_else"
  val desc = "(ZZ)V"
  val insns = {
    val lbl0 = label()
    val lbl1 = label()
    val lbl2 = label()
    insnList(
      iload(0),
      ifeq(lbl0),
      invokestatic("foo", "bar", "()V"),
      goto(lbl2),
      lbl0,
      iload(1),
      ifeq(lbl1),
      invokestatic("foo", "baz", "()V"),
      goto(lbl2),
      lbl1,
      invokestatic("foo", "qux", "()V"),
      lbl2,
      vreturn())
  }

  val test: Test = {
    case Exec(If(True(_), Then(_),
			  Else(Label(_) ::
			       If(True(_), Then(_),
					   Else(_)) :: Nil)) :: _) => true
    case tree => false
  }
}
