package scala.bytecode.test.ifs

import scala.bytecode.asm._
import scala.bytecode.ast._

object if__if_elif_else__elif_else extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 4
  val name = "if__if_elif_else__elif_else"
  val desc = "(ZZZZ)V"
  val insns = {
    val lbl0 = label()
    val lbl1 = label()
    val lbl2 = label()
    val lbl3 = label()
    val lbl4 = label()
    insnList(
      iload(0),
      ifeq(lbl2),
      iload(1),
      ifeq(lbl0),
      invokestatic("foo", "bar", "()V"),
      goto(lbl4),
      lbl0,
      iload(2),
      ifeq(lbl1),
      invokestatic("foo", "baz", "()V"),
      goto(lbl4),
      lbl1,
      invokestatic("foo", "qux", "()V"),
      goto(lbl4),
      lbl2,
      iload(3),
      ifeq(lbl3),
      invokestatic("foo", "quux", "()V"),
      goto(lbl4),
      lbl3,
      invokestatic("foo", "thud", "()V"),
      lbl4,
      vreturn())
  }
/*javap
       0: iload_1       
       1: ifeq          33
       4: iload_2       
       5: ifeq          15
       8: iconst_0      
       9: invokestatic  #2                  // Method java/lang/System.exit:(I)V
      12: goto          49
      15: iload_3       
      16: ifeq          26
      19: iconst_0      
      20: invokestatic  #2                  // Method java/lang/System.exit:(I)V
      23: goto          49
      26: iconst_1      
      27: invokestatic  #2                  // Method java/lang/System.exit:(I)V
      30: goto          49
      33: iload         4
      35: ifeq          45
      38: iconst_0      
      39: invokestatic  #2                  // Method java/lang/System.exit:(I)V
      42: goto          49
      45: iconst_1      
      46: invokestatic  #2                  // Method java/lang/System.exit:(I)V
      49: return*/
  val test: Test = {
    case Exec(If(True(_),
		 Then(If(True(_),
			 Then(Void(Method(_, "bar", _, _, _)) :: _),
			 Else(Label(_) ::
			      If(True(_),
				 Then(Void(Method(_, "baz", _, _, _)) :: _),
				 Else(Label(_) ::
				      Void(Method(_, "qux", _, _, _)) :: _)) ::
			      _)) :: Nil),
		 Else(Label(_) ::
		      If(True(_),
			 Then(Void(Method(_, "quux", _, _, _)) :: _),
			 Else(Label(_) ::
			      Void(Method(_, "thud", _, _, _)) :: _)) ::
		      Nil)) ::
	      Label(_) ::
	      Return(_) ::
	      Nil) => true
    case tree => false
  }
}
