package scala.bytecode.test.ifs

import scala.bytecode.asm._
import scala.bytecode.ast._

object if_if__if extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 3
  val name = "if_if__if"
  val desc = "(ZZZ)V"
  val insns = {
    val lbl0 = label()
    val lbl1 = label()
/*     0: iload_0       
       1: ifeq          20
       4: iload_1       
       5: ifeq          12
       8: iconst_1      
       9: invokestatic  #2                  // Method java/lang/System.exit:(I)V
      12: iload_2       
      13: ifeq          20
      16: iconst_2      
      17: invokestatic  #2                  // Method java/lang/System.exit:(I)V
      20: return  */
    insnList(
      iload(0),
      ifeq(lbl1),
      iload(1),
      ifeq(lbl0),
      invokestatic("foo", "bar", "()V"),
      lbl0,
      iload(2),
      ifeq(lbl1),
      invokestatic("foo", "baz", "()V"),
      lbl1,
      vreturn())
  }

  val test: Test = {
    case Exec(If(True(_),
		 Then(
		   If(True(_),
		      Then(Void(Method(_, "bar", _, _, _)) :: _)) ::
		   Label(_) ::
		   If(True(_),
		      Then(Void(Method(_, "baz", _, _, _)) :: _)) :: Nil)) ::
	      Label(_) ::
	      Return(_) ::
	      Nil) => true
    case tree => false
  }
}
