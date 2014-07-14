package scala.bytecode.test.ifs

import scala.bytecode.asm._
import scala.bytecode.ast._

object org_objectweb_asm_Frame_merge_cw_t_types_index
extends scala.bytecode.test.ASTCase {
  val maxStack = 5
  val maxLocals = 7
  val name = "org_objectweb_asm_Frame_merge_cw_t_types_index"
  val desc = "(Lorg/objectweb/asm/ClassWriter;I[II)Z"
  val insns = {
    val lbl0 = label()
    val lbl1 = label()
    val lbl2 = label()
    val lbl3 = label()
    val lbl4 = label()
    val lbl5 = label()
    val lbl6 = label()
    val lbl7 = label()
    val lbl8 = label()
    val lbl9 = label()
    val lbl10 = label()
    val lbl11 = label()
    val lbl12 = label()
    val lbl13 = label()
    val lbl14 = label()
    val lbl15 = label()
    val lbl16 = label()
    insnList(
//#0 0..7
      aload(2),
      iload(3),
      array.iload(),
      istore(4),
      iload(4),
      iload(1),
      if_icmpne(lbl0),
//#1 7..9
      push(0),
      ireturn(),
//#2 9..15
      lbl0,
      iload(1),
      push(268435455),
      iand(),
      push(16777221),
      if_icmpne(lbl2),
//#3 15..18
      iload(4),
      push(16777221),
      if_icmpne(lbl1),
//#4 18..20
      push(0),
      ireturn(),
//#5 20..23
      lbl1,
      push(16777221),
      istore(1),
//#6
      lbl2,
      iload(4),
      ifne(lbl3),
//#7
      aload(2),
      iload(3),
      iload(1),
      array.istore(),
      push(1),
      ireturn(),
//#8
      lbl3,
      iload(4),
      push(267386880),
      iand(),
      push(24117248),
      if_icmpeq(lbl4),
//#9
      iload(4),
      push(-268435456),
      iand(),
      ifeq(lbl10),
//#10
      lbl4,
      iload(1),
      push(16777221),
      if_icmpne(lbl5),
//#11
      push(0),
      ireturn(),
//#12
      lbl5,
      iload(1),
      push(-1048576),
      iand(),
      iload(4),
      push(-1048576),
      iand(),
      if_icmpne(lbl7),
//#13
      iload(4),
      push(267386880),
      iand(),
      push(24117248),
      if_icmpne(lbl6),
//#14
      iload(1),
      push(-268435456),
      iand(),
      push(24117248),
      ior(),
      aload(0),
      iload(1),
      push(1048575),
      iand(),
      iload(4),
      push(1048575),
      iand(),
      invokevirtual("org/objectweb/asm/ClassWriter", "a", "(II)I"),
      ior(),
      istore(5),
      goto(lbl15),
//#15
      lbl6,
      push(24117248),
      aload(0),
      push("java/lang/Object"),
      invokevirtual("org/objectweb/asm/ClassWriter", "c", "(Ljava/lang/String;)I"),
      ior(),
      istore(5),
      goto(lbl15),
//#16
      lbl7,
      iload(1),
      push(267386880),
      iand(),
      push(24117248),
      if_icmpeq(lbl8),
//#17
      iload(1),
      push(-268435456),
      iand(),
      ifeq(lbl9),
//#18
      lbl8,
      push(24117248),
      aload(0),
      push("java/lang/Object"),
      invokevirtual("org/objectweb/asm/ClassWriter", "c", "(Ljava/lang/String;)I"),
      ior(),
      istore(5),
      goto(lbl15),
//#19
      lbl9,
      push(16777216),
      istore(5),
      goto(lbl15),
//#20
      lbl10,
      iload(4),
      push(16777216),
      if_icmpne(lbl14),
//#21
      iload(1),
      push(267386880),
      iand(),
      push(24117248),
      if_icmpeq(lbl11),
//#22
      iload(1),
      push(-268435456),
      iand(),
      ifeq(lbl12),
//#23
      lbl11,
      iload(1),
      goto(lbl13),
//#24
      lbl12,
      push(16777216),
//#25
      lbl13,
      istore(5),
      goto(lbl15),//201->208
//#26
      lbl14,
      push(16777216),//204
      istore(5),
//#27
      lbl15,//208
      iload(4),
      iload(5),
      if_icmpeq(lbl16),//212->222
//#28
      aload(2),
      iload(3),
      iload(5),
      array.istore(),
      push(1),
      ireturn(),
//#29
      lbl16,//222
      push(0),
      ireturn())
  }

  val test: Test = {
    case Exec(Store(_, _) ::
	      If(_,
		 Then(Return(_) ::
		      Nil)) ::
	      Label(_) ::
	      If(_,
		 Then(If(_,
			 Then(Return(_) ::
			      Nil)) ::
		      Label(_) ::
		      Store(_, _) ::
		      Nil)) ::
	      Label(_) ::
	      If(_,
		 Then(ArrayStore(_, _, _) ::
		      Return(_) ::
		      Nil)) ::
	      Label(_) ::
	      If(Or(_, _),
		 Then(Label(_) ::
		      If(_,
			 Then(Return(_) ::
			      Nil)) ::
		      Label(_) ::
		      If(_,
			 Then(If(_,
				 Then(Store(_, _) ::
				      Goto(_, _) ::
				      Nil),
				 Else(Label(_) ::
				      Store(_, _) ::
				      Goto(_, _) ::
				      Nil)) ::
			      Nil),
			 Else(Label(_) ::
			      If(Or(_, _),
				 Then(Label(_) ::
				      Store(_, _) ::
				      Goto(_, _) ::
				      Nil),
				 Else(Label(_) ::
				      Store(_, _) ::
				      Goto(_, _) ::
				      Nil)) ::
			      Nil)) ::
		      Nil),
		 Else(_)) ::
	      Label(_) ::
	      If(_,
		 Then(ArrayStore(_, _, _) ::
		      Return(_) ::
		      Nil)) ::
	      Label(_) ::
	      Return(_) ::
	      _) => true
    case tree => false
  }
}
