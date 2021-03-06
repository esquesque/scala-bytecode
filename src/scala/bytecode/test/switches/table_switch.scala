package scala.bytecode.test.switches

import scala.bytecode.asm._
import scala.bytecode.ast._

object table_switch extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 1
  val name = "table_switch"
  val desc = "(I)V"
  val insns = {
    val lbl0 = label()
    val lbl1 = label()
    val lbl2 = label()
    val lbl3 = label()
    val lbl4 = label()
    val lbl5 = label()
    val default = label()
    val exit = label()
    insnList(
      iload(0),
      tableswitch(0xa, 0xf, default, List(lbl0, lbl1, lbl2, lbl3, lbl4, lbl5)),
      lbl0,
      invokestatic("foo", "bar", "()V"),
      goto(exit),
      lbl1,
      invokestatic("foo", "baz", "()V"),
      goto(exit),
      lbl2,
      invokestatic("foo", "qux", "()V"),
      goto(exit),
      lbl3,
      invokestatic("foo", "quux", "()V"),
      goto(exit),
      lbl4,
      invokestatic("foo", "doop", "()V"),
      goto(exit),
      lbl5,
      invokestatic("foo", "dodo", "()V"),
      goto(exit),
      default,
      invokestatic("foo", "thud", "()V"),
      exit,
      vreturn())
  }

  val test: Test = {
    case Exec(Switch(_, cases, _) :: _ :: Return(None) :: Nil) =>
      cases.length == 6
    case tree => false
  }
}
