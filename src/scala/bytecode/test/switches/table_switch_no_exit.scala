package scala.bytecode.test.switches

import scala.bytecode.asm._
import scala.bytecode.ast._

object table_switch_no_exit extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 1
  val name = "table_switch_no_exit"
  val desc = "(I)V"
  val insns = {
    val lbl0 = label()
    val lbl1 = label()
    val lbl2 = label()
    val lbl3 = label()
    val lbl4 = label()
    val lbl5 = label()
    val lbl6 = label()
    val default = label()
    val exit = label()
    insnList(
      iload(0),
      tableswitch(0xa, 0xf, default, List(lbl0, lbl1, lbl2, lbl3, lbl4, lbl5, default)),
      lbl0,
      invokestatic("foo", "bar", "()V"),
      vreturn(),
      lbl1,
      invokestatic("foo", "baz", "()V"),
      vreturn(),
      lbl2,
      invokestatic("foo", "qux", "()V"),
      vreturn(),
      lbl3,
      invokestatic("foo", "quux", "()V"),
      vreturn(),
      lbl4,
      invokestatic("foo", "doop", "()V"),
      vreturn(),
      lbl5,
      invokestatic("foo", "dodo", "()V"),
      vreturn(),
      default,
      invokestatic("foo", "thud", "()V"),
      lbl6,
      iload(0),
      ifeq(exit),
      invokestatic("infinite", "loop", "()V"),
      goto(lbl6),
      exit,
      vreturn())
  }

  val test: Test = {
    case Exec(Switch(_, cases, _) :: _ :: While(_, _) :: _ :: _ :: Nil) =>
      cases.length == 6
    case tree => false
  }
}
