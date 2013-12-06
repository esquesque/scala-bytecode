/*Facile bytecode analysis, disassembling, and manipulation in Scala.
 *Copyright (C) 2013  David Phillips
 *
 *This program is free software: you can redistribute it and/or modify
 *it under the terms of the GNU General Public License as published by
 *the Free Software Foundation, either version 3 of the License, or
 *(at your option) any later version.
 *
 *This program is distributed in the hope that it will be useful,
 *but WITHOUT ANY WARRANTY; without even the implied warranty of
 *MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *GNU General Public License for more details.
 *
 *You should have received a copy of the GNU General Public License
 *along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package scala.bytecode

import org.objectweb.asm.{Opcodes, tree, util}

package object asm {
  import Opcodes._
  //alias so the extractor object names won't collide (for continuity w/ ASM)
  import tree.{ClassNode, FieldNode, MethodNode, InsnList,
	       AbstractInsnNode,
	       InsnNode => insnNode,
	       IntInsnNode => intInsnNode,
	       VarInsnNode => varInsnNode,
	       TypeInsnNode => typeInsnNode,
	       FieldInsnNode => fieldInsnNode,
	       MethodInsnNode => methodInsnNode,
	       JumpInsnNode => jumpInsnNode,
	       LabelNode => labelNode,
	       IincInsnNode => iincInsnNode,
	       TableSwitchInsnNode,//todo
	       LookupSwitchInsnNode,//todo
	       MultiANewArrayInsnNode}//todo
  import util.Printer.{OPCODES => MNEMS}

  type Insn = AbstractInsnNode

  object nop extends InsnX(NOP)

  def insnList(insns: Insn*): RichInsnList = {
    val insnList = new InsnList()
    insns foreach (insnList add _)
    new RichInsnList(insnList)
  }

  val EMPTY_MAP: java.util.Map[Any, Any] = new java.util.HashMap[Any, Any]
  def insnClone(insn: Insn): Insn = insn.clone(EMPTY_MAP)

  /* @return whether insn has side effects.
   */
  def insnHasSideEffects(insn: Insn): Boolean = insn match {
    case store(_, _) => true
    case IincInsnNode(_, _) => true
    case putstatic(_, _, _) => true
    case putfield(_, _, _) => true
    case MethodInsnNode(_, _, _, _) => true
    case _ => false
  }

  /* @return mnemonic of insn.
   */
  def insnName(insn: Insn): String = insn match {
    case asm.LabelNode(_) => "0x"+ insn.hashCode.toHexString
    case asm.JumpInsnNode(opcode, lbl) =>
      MNEMS(opcode).toLowerCase +"->"+ insnName(lbl)
    case _ => MNEMS(insn.getOpcode).toLowerCase
  }

  /* @return whether insn pushes the stack.
   */
  def insnPushes(insn: Insn): Boolean = insn match {
    case push(_) => true
    case load(_, _) => true
    //case array.load(_) => true
    //case array.alloc(_) => true
    case math(_, _, _) => true
    case cast(_, _) => true
    case getstatic(_, _, _) => true
    case getfield(_, _, _) => true
    case anew(_) => true
    case initnew(_, _, desc) => ! (desc endsWith "V")
    case MethodInsnNode(_, _, _, desc) => ! (desc endsWith "V")
    case _ => false
  }

  /* @return an informative string representation of insn.
   */
  def insnString(insn: Insn): String = insn match {
    case ipush(i) => "ipush "+ i
    case lpush(l) => "lpush "+ l
    case fpush(f) => "fpush "+ f
    case dpush(d) => "dpush "+ d
    case apush(a: String) => "apush \""+ a +'"'
    case apush(a) => "apush "+ a
    case InsnNode(_) => insnName(insn)
    case array.inew() => "newarray [I"
    case array.lnew() => "newarray [J"
    case array.fnew() => "newarray [F"
    case array.dnew() => "newarray [D"
    case array.anew(desc) => "anewarray [L"+ desc +";"
    case array.bnew() => "newarray [B"
    case array.cnew() => "newarray [C"
    case array.snew() => "newarray [S"
    case array.znew() => "newarray [Z"
    case IntInsnNode(_, operand) => insnName(insn) +" "+ operand
    case VarInsnNode(_, varidx) => insnName(insn) +" #"+ varidx
    case IincInsnNode(varidx, incrmt) =>
      "iinc #" + varidx.toString +
      (if (incrmt < 0) "-=" else "+=") +
      (if (incrmt < 0) -incrmt else incrmt)
    case TypeInsnNode(_, desc) => insnName(insn) +" "+ desc
    case FieldInsnNode(_, owner, name, desc) =>
      insnName(insn) +" "+ owner +"/"+ name +" "+ desc
    case InitNewNode(inst, owner, desc) =>
      "initnew "+ inst +" "+ owner + desc
    case MethodInsnNode(_, owner, name, desc) =>
      insnName(insn) +" "+ owner +"/"+ name + desc
    case JumpInsnNode(_, target) => insnName(insn)
    case LabelNode(label) => "label~"+ insnName(insn)
    case _ => insn.toString +"~"+ insnName(insn)
  }

  sealed trait X {
    def unapply(insn: Insn): Any

    def any: Insn => Boolean = unapply(_) match {
      case false => false
      case None => false
      case _ => true
    }
  }

  class InsnX(op: Int) extends X {
    def apply(): insnNode = InsnNode(op)
    def unapply(insn: Insn): Boolean = insn.getOpcode == op
  }

  class NewarrayX(t: Int) extends X {
    def apply(): intInsnNode = IntInsnNode(NEWARRAY, t)

    def unapply(insn: Insn): Boolean = insn match {
      case IntInsnNode(NEWARRAY, x) => x == t; case _ => false
    }
  }

  class VarInsnX(op: Int) extends X {
    def apply(v: Int): varInsnNode = VarInsnNode(op, v)

    def unapply(insn: Insn): Option[Int] = insn match {
      case VarInsnNode(x, v) if x == op => Some(v); case _ => None
    }
  }

  class VarLoadX(op: Int) extends VarInsnX(op)
  class VarStoreX(op: Int) extends VarInsnX(op)

  class TypeInsnX(op: Int) extends X {
    def apply(desc: String): typeInsnNode = TypeInsnNode(op, desc)
    def unapply(insn: Insn): Option[String] = insn match {
      case TypeInsnNode(x, desc) if x == op => Some(desc)
      case _ => None
    }
  }

  trait MemberX extends X {
    def unapply(insn: Insn): Option[(String, String, String)]
    def byOwner(owner: String): Insn => Boolean = unapply(_) match {
      case Some((owner0, _, _)) => owner0 equals owner
      case None => false
    }
    def byName(name: String): Insn => Boolean = unapply(_) match {
      case Some((_, name0, _)) => name equals name0
      case None => false
    }
    def byDesc(desc: String): Insn => Boolean = unapply(_) match {
      case Some((_, _, desc0)) => desc equals desc0
      case None => false
    }
  }

  class FieldInsnX(op: Int) extends MemberX {
    def apply(owner: String, name: String, desc: String): fieldInsnNode =
      FieldInsnNode(op, owner, name, desc)

    def unapply(insn: Insn): Option[(String, String, String)] = insn match {
      case FieldInsnNode(op0, owner, name, desc) if op0 == op =>
	Some(owner, name, desc)
      case _ => None
    }
  }

  class MethodInsnX(op: Int) extends MemberX {
    def apply(owner: String, name: String, desc: String): methodInsnNode =
      MethodInsnNode(op, owner, name, desc)

    def unapply(insn: Insn): Option[(String, String, String)] = insn match {
      case MethodInsnNode(op0, owner, name, desc) if op0 == op =>
	Some(owner, name, desc)
      case _ => None
    }
  }

  trait JumpX extends X {
    def unapply(insn: Insn): Option[labelNode]
  }

  class JumpInsnX(op: Int) extends JumpX {
    def apply(lbl: labelNode): jumpInsnNode = JumpInsnNode(op, lbl)

    def unapply(insn: Insn): Option[labelNode] = insn match {
      case JumpInsnNode(op0, lbl) if op0 == op => Some(lbl)
      case _ => None
    }
  }

  object label extends X {
    def unapply(insn: Insn): Boolean = insn.isInstanceOf[labelNode]
  }

  object insns {
    def println(insn: Insn, desc: String): List[Insn] = List(
      getstatic("java/lang/System", "out", "Ljava/io/PrintStream;"), insn,
      invokevirtual("java/io/PrintStream", "println", "("+ desc +")V"))

    def println(insns: List[Insn], desc: String): List[Insn] =
      getstatic("java/lang/System", "out", "Ljava/io/PrintStream;") :: insns ++
      (invokevirtual("java/io/PrintStream", "println", "("+ desc +")V") :: Nil)
  }

  // primitive push series 0x01-0x14

  object push extends X {
    def apply(i: Int): Insn = ipush(i)
    def apply(l: Long): Insn = lpush(l)
    def apply(f: Float): Insn = fpush(f)
    def apply(d: Double): Insn = dpush(d)
    def apply(a: AnyRef): Insn = apush(a)

    def unapply(insn: Insn): Option[Any] = insn match {
      case ipush(i) => Some(i)
      case lpush(l) => Some(l)
      case fpush(f) => Some(f)
      case dpush(d) => Some(d)
      case apush(a) => Some(a)
      case _ => None
    }
  }

  object ipush extends X {
    def apply(i: Int): Insn = i match {
      case c if c >= -1 && c <= 5 => asm.InsnNode(ICONST_0 + c)
      case b if b >= -128 && b <= 127 => asm.IntInsnNode(BIPUSH, b)
      case s if s >= -32768 && s <= 32767 => asm.IntInsnNode(SIPUSH, s)
      case _ => LdcInsnNode(i)
    }

    def unapply(insn: Insn): Option[Int] = insn match {
      case InsnNode(op) if op >= ICONST_M1 && op <= ICONST_5 =>
	Some(op - ICONST_0)
      case IntInsnNode(op, operand) if op == BIPUSH || op == SIPUSH =>
	Some(operand)
      case LdcInsnNode(cst: Int) => Some(cst)
      case _ => None
    }
  }

  object lpush extends X {
    def apply(l: Long): Insn = l match {
      case c if c == 0 || c == 1 => asm.InsnNode(LCONST_0 + c.toInt)
      case _ => asm.LdcInsnNode(l)
    }

    def unapply(insn: Insn): Option[Long] = insn match {
      case InsnNode(op) if op == LCONST_0 || op == LCONST_1 =>
	Some(op - LCONST_0)
      case LdcInsnNode(cst: Long) => Some(cst)
      case _ => None
    }
  }

  object fpush extends X {
    def apply(f: Float): Insn = f match {
      case c if c >= 0 && c <= 2 => asm.InsnNode(FCONST_0 + c.toInt)
      case _ => asm.LdcInsnNode(f)
    }

    def unapply(insn: Insn): Option[Float] = insn match {
      case InsnNode(op) if op >= FCONST_0 && op <= FCONST_2 =>
	Some(op - FCONST_0)
      case LdcInsnNode(cst: Float) => Some(cst)
      case _ => None
    }
  }

  object dpush extends X {
    def apply(d: Double): Insn = d match {
      case c if c == 0 || c == 1 => asm.InsnNode(DCONST_0 + c.toInt)
      case _ => asm.LdcInsnNode(d)
    }

    def unapply(insn: Insn): Option[Double] = insn match {
      case InsnNode(op) if op == DCONST_0 || op == DCONST_1 =>
	Some(op - DCONST_0)
      case LdcInsnNode(cst: Double) => Some(cst)
      case _ => None
    }
  }

  object apush extends X {
    def apply(a: AnyRef): Insn = a match {
      case null => asm.InsnNode(ACONST_NULL)
      case _ => asm.LdcInsnNode(a)
    }

    def unapply(insn: Insn): Option[AnyRef] = insn match {
      case InsnNode(ACONST_NULL) => Some(null)//hahaha
      case LdcInsnNode(cst: AnyRef) => Some(cst)
      case _ => None
    }
  }

  // var load series 0x15-0x2d

  object load extends X {
    def apply(v: Int, desc: Option[String]): Insn = desc match {
      case Some("I") => iload(v); case Some("J") => lload(v)
      case Some("F") => fload(v); case Some("D") => dload(v)
      case _ => aload(v)
    }

    def unapply(insn: Insn): Option[(Int, Option[String])] = insn match {
      case iload(v) => Some(v, Some("I"))
      case lload(v) => Some(v, Some("J"))
      case fload(v) => Some(v, Some("F"))
      case dload(v) => Some(v, Some("D"))
      case aload(v) => Some(v, None)
      case _ => None
    }
  }

  object iload extends VarLoadX(ILOAD)
  object lload extends VarLoadX(LLOAD)
  object fload extends VarLoadX(FLOAD)
  object dload extends VarLoadX(DLOAD)
  object aload extends VarLoadX(ALOAD)

  // array series 0x2e-0x35, 0x4f-0x56, 0xbc-0xbe, 0xc5

  object array {
    object load extends X {
      def apply(desc: Option[String]): Insn = desc match {
	case Some("I") => array.iload(); case Some("J") => array.lload()
	case Some("F") => array.fload(); case Some("D") => array.dload()
        case Some("B") => array.bload(); case Some("C") => array.cload()
	case Some("S") => array.sload(); case _         => array.aload()
      }

      def unapply(insn: Insn): Option[Option[String]] = insn match {
	case array.iload() => Some(Some("I"))
	case array.lload() => Some(Some("J"))
	case array.fload() => Some(Some("F"))
	case array.dload() => Some(Some("D"))
	case array.aload() => Some(None)
	case array.bload() => Some(Some("B"))
	case array.cload() => Some(Some("C"))
	case array.sload() => Some(Some("S"))
	case _ => None
      }
    }

    object iload extends InsnX(IALOAD)
    object lload extends InsnX(LALOAD)
    object fload extends InsnX(FALOAD)
    object dload extends InsnX(DALOAD)
    object aload extends InsnX(AALOAD)
    object bload extends InsnX(BALOAD)
    object cload extends InsnX(CALOAD)
    object sload extends InsnX(SALOAD)

    object store extends X {
      def apply(desc: Option[String]): Insn = desc match {
	case Some("I")  => array.istore(); case Some("J") => array.lstore()
	case Some("F")  => array.fstore(); case Some("D") => array.dstore()
        case Some("B") => array.bstore();  case Some("C") => array.cstore()
	case Some("S") => array.sstore();  case _         => array.astore()
      }

      def unapply(insn: Insn): Option[Option[String]] = insn match {
	case array.istore() => Some(Some("I"))
	case array.lstore() => Some(Some("J"))
	case array.fstore() => Some(Some("F"))
	case array.dstore() => Some(Some("D"))
	case array.astore() => Some(None)
	case array.bstore() => Some(Some("B"))
	case array.cstore() => Some(Some("C"))
	case array.sstore() => Some(Some("S"))
	case _              => None
      }
    }

    object istore extends InsnX(IASTORE)
    object lstore extends InsnX(LASTORE)
    object fstore extends InsnX(FASTORE)
    object dstore extends InsnX(DASTORE)
    object astore extends InsnX(AASTORE)
    object bstore extends InsnX(BASTORE)
    object cstore extends InsnX(CASTORE)
    object sstore extends InsnX(SASTORE)

    object alloc extends X {
      def apply(desc: String): Insn = desc match {
	case "[I" => array.inew(); case "[J" => array.lnew()
	case "[F" => array.fnew(); case "[D" => array.dnew()
        case "[B" => array.bnew(); case "[C" => array.cnew()
	case "[S" => array.snew(); case "[Z" => array.znew()
	case desc =>
	  array.anew(desc)
      }

      def unapply(insn: Insn): Option[String] = insn match {
	case array.inew()     => Some("[I"); case array.lnew() => Some("[J")
	case array.fnew()     => Some("[F"); case array.dnew() => Some("[D")
	case array.anew(desc) => Some(desc); case array.bnew() => Some("[B")
	case array.cnew()     => Some("[C"); case array.snew() => Some("[S")
	case array.znew() => Some("[Z")
	case _ =>
	  None
      }
    }

    object inew extends NewarrayX(T_INT)
    object lnew extends NewarrayX(T_LONG)
    object fnew extends NewarrayX(T_FLOAT)
    object dnew extends NewarrayX(T_DOUBLE)
    object anew extends TypeInsnX(ANEWARRAY)
    object bnew extends NewarrayX(T_BYTE)
    object cnew extends NewarrayX(T_CHAR)
    object snew extends NewarrayX(T_SHORT)
    object znew extends NewarrayX(T_BOOLEAN)

    object length extends InsnX(ARRAYLENGTH)

    object multianew//...............
  }

  // var store series 0x36-0x4e

  object store extends X {
    def apply(v: Int, desc: Option[String]): Insn = desc match {
      case Some("I") => istore(v); case Some("J") => lstore(v)
      case Some("F") => fstore(v); case Some("D") => dstore(v)
      case _ => astore(v)
    }

    def unapply(insn: Insn): Option[(Int, Option[String])] = insn match {
      case istore(v) => Some(v, Some("I"))
      case lstore(v) => Some(v, Some("J"))
      case fstore(v) => Some(v, Some("F"))
      case dstore(v) => Some(v, Some("D"))
      case astore(v) => Some(v, None)
      case _ => None
    }
  }

  object istore extends VarStoreX(ISTORE)
  object lstore extends VarStoreX(LSTORE)
  object fstore extends VarStoreX(FSTORE)
  object dstore extends VarStoreX(DSTORE)
  object astore extends VarStoreX(ASTORE)

  // stack manipulation series 0x57-0x5f

  object pop extends InsnX(POP)
  object pop2 extends InsnX(POP2)

  object dup extends InsnX(DUP)
  object dup_x1 extends InsnX(DUP_X1)
  object dup_x2 extends InsnX(DUP_X2)

  object dup2 extends InsnX(DUP2)
  object dup2_x1 extends InsnX(DUP2_X1)
  object dup2_x2 extends InsnX(DUP2_X2)

  object swap extends InsnX(SWAP)

  // arithmetic series 0x60-0x83

  object math extends X {
    def unapply(insn: Insn): Option[(String, String, Int)] = insn match {
      case iadd() =>  Some("I", "+",   2); case ladd() =>  Some("J", "+",   2)
      case fadd() =>  Some("F", "+",   2); case dadd() =>  Some("D", "+",   2)
      case isub() =>  Some("I", "-",   2); case lsub() =>  Some("J", "-",   2)
      case fsub() =>  Some("F", "-",   2); case dsub() =>  Some("D", "-",   2)
      case imul() =>  Some("I", "*",   2); case lmul() =>  Some("J", "*",   2)
      case fmul() =>  Some("F", "*",   2); case dmul() =>  Some("D", "*",   2)
      case idiv() =>  Some("I", "/",   2); case ldiv() =>  Some("J", "/",   2)
      case fdiv() =>  Some("F", "/",   2); case ddiv() =>  Some("D", "/",   2)
      case irem() =>  Some("I", "%",   2); case lrem() =>  Some("J", "%",   2)
      case frem() =>  Some("F", "%",   2); case drem() =>  Some("D", "%",   2)
      case ineg() =>  Some("I", "-",   1); case lneg() =>  Some("J", "-",   1)
      case fneg() =>  Some("F", "-",   1); case dneg() =>  Some("D", "-",   1)
      case ishl() =>  Some("I", "<<",  2); case lshl() =>  Some("J", "<<",  2)
      case ishr() =>  Some("I", ">>",  2); case lshr() =>  Some("J", ">>",  2)
      case iushr() => Some("I", ">>>", 2); case lushr() => Some("J", ">>>", 2)
      case iand() =>  Some("I", "&",   2); case land() =>  Some("J", "&",   2)
      case ior() =>   Some("I", "|",   2); case lor() =>   Some("J", "|",   2)
      case ixor() =>  Some("I", "^",   2); case lxor() =>  Some("J", "^",   2)
      case _ => None
    }
  }

  object iadd extends InsnX(IADD)
  object ladd extends InsnX(LADD)
  object fadd extends InsnX(FADD)
  object dadd extends InsnX(DADD)

  object isub extends InsnX(ISUB)
  object lsub extends InsnX(LSUB)
  object fsub extends InsnX(FSUB)
  object dsub extends InsnX(DSUB)

  object imul extends InsnX(IMUL)
  object lmul extends InsnX(LMUL)
  object fmul extends InsnX(FMUL)
  object dmul extends InsnX(DMUL)

  object idiv extends InsnX(IDIV)
  object ldiv extends InsnX(LDIV)
  object fdiv extends InsnX(FDIV)
  object ddiv extends InsnX(DDIV)

  object irem extends InsnX(IREM)
  object lrem extends InsnX(LREM)
  object frem extends InsnX(FREM)
  object drem extends InsnX(DREM)

  object ineg extends InsnX(INEG)
  object lneg extends InsnX(LNEG)
  object fneg extends InsnX(FNEG)
  object dneg extends InsnX(DNEG)

  object ishl extends InsnX(ISHL)
  object lshl extends InsnX(LSHL)

  object ishr extends InsnX(ISHR)
  object lshr extends InsnX(LSHR)

  object iushr extends InsnX(IUSHR)
  object lushr extends InsnX(LUSHR)

  object iand extends InsnX(IAND)
  object land extends InsnX(LAND)

  object ior extends InsnX(IOR)
  object lor extends InsnX(LOR)

  object ixor extends InsnX(IXOR)
  object lxor extends InsnX(LXOR)

  // iinc 0x84

  object iinc extends X {
    def apply(v: Int, n: Int): iincInsnNode = IincInsnNode(v, n)
    def unapply(insn: Insn): Option[(Int, Int)] = insn match {
      case IincInsnNode(v, n) => Some((v, n))
      case _ => None
    }
  }

  // conversion series 0x85-0x93, 0xc0

  object cast extends X {
    def unapply(insn: Insn): Option[(Option[String], String)] = insn match {
      case i2l() => Some(Some("I"), "J"); case i2f() => Some(Some("I"), "F")
      case i2d() => Some(Some("I"), "D"); case l2i() => Some(Some("J"), "I")
      case l2f() => Some(Some("J"), "F"); case l2d() => Some(Some("J"), "D")
      case f2i() => Some(Some("F"), "I"); case f2l() => Some(Some("F"), "J")
      case f2d() => Some(Some("F"), "D"); case d2i() => Some(Some("D"), "I")
      case d2l() => Some(Some("D"), "J"); case d2f() => Some(Some("D"), "F")
      case checkcast(desc) => Some(None, desc)
      case _ => None
    }
  }

  object i2l extends InsnX(I2L)
  object i2f extends InsnX(I2F)
  object i2d extends InsnX(I2D)

  object l2i extends InsnX(L2I)
  object l2f extends InsnX(L2F)
  object l2d extends InsnX(L2D)

  object f2i extends InsnX(F2I)
  object f2l extends InsnX(F2L)
  object f2d extends InsnX(F2D)

  object d2i extends InsnX(D2I)
  object d2l extends InsnX(D2L)
  object d2f extends InsnX(D2F)

  object checkcast extends TypeInsnX(CHECKCAST)

  // compare/push series 0x94-0x98

  object cmp extends X {
    def unapply(insn: Insn): Option[String] = insn match {
      case lcmp() => Some("J")
      case fcmpl() => Some("F"); case fcmpg() => Some("F")
      case dcmpl() => Some("D"); case dcmpg() => Some("D")
      case _ => None
    }
  }

  object lcmp extends InsnX(LCMP)
  object fcmpl extends InsnX(FCMPL)
  object fcmpg extends InsnX(FCMPG)
  object dcmpl extends InsnX(DCMPL)
  object dcmpg extends InsnX(DCMPG)

  // compare/branch series 0x9f-0xa6, 0xc6 & 0xc7

  object ifxx extends JumpX {
    def unapply(insn: Insn): Option[labelNode] = insn match {
      case ifeq(lbl) => Some(lbl); case ifne(lbl) => Some(lbl)
      case iflt(lbl) => Some(lbl); case ifge(lbl) => Some(lbl)
      case ifgt(lbl) => Some(lbl); case ifle(lbl) => Some(lbl)
      case _ => None
    }
  }

  object ifeq extends JumpInsnX(IFEQ)
  object ifne extends JumpInsnX(IFNE)
  object iflt extends JumpInsnX(IFLT)
  object ifge extends JumpInsnX(IFGE)
  object ifgt extends JumpInsnX(IFGT)
  object ifle extends JumpInsnX(IFLE)

  object if_icmpxx extends JumpX {
    def unapply(insn: Insn): Option[labelNode] = insn match {
      case if_icmpeq(lbl) => Some(lbl); case if_icmpne(lbl) => Some(lbl)
      case if_icmplt(lbl) => Some(lbl); case if_icmpge(lbl) => Some(lbl)
      case if_icmpgt(lbl) => Some(lbl); case if_icmple(lbl) => Some(lbl)
      case _ => None
    }
  }

  object if_icmpeq extends JumpInsnX(IF_ICMPEQ)
  object if_icmpne extends JumpInsnX(IF_ICMPNE)
  object if_icmplt extends JumpInsnX(IF_ICMPLT)
  object if_icmpge extends JumpInsnX(IF_ICMPGE)
  object if_icmpgt extends JumpInsnX(IF_ICMPGT)
  object if_icmple extends JumpInsnX(IF_ICMPLE)

  object if_acmpeq extends JumpInsnX(IF_ACMPEQ)
  object if_acmpne extends JumpInsnX(IF_ACMPNE)

  object ifnull extends JumpInsnX(IFNULL)
  object ifnonnull extends JumpInsnX(IFNONNULL)

  // goto & jsr/ret 0xa7-0xa9

  object goto extends JumpInsnX(GOTO)
  object jsr extends JumpInsnX(JSR)
  object ret extends VarInsnX(RET)

  // switch series 0xaa & 0xab

  object tableswitch
  object lookupswitch

  // return series

  object ireturn extends InsnX(IRETURN)
  object lreturn extends InsnX(LRETURN)
  object freturn extends InsnX(FRETURN)
  object dreturn extends InsnX(DRETURN)
  object areturn extends InsnX(ARETURN)
  object vreturn extends InsnX(RETURN)

  // field series

  object getstatic extends FieldInsnX(GETSTATIC)
  object putstatic extends FieldInsnX(PUTSTATIC)
  object getfield extends FieldInsnX(GETFIELD)
  object putfield extends FieldInsnX(PUTFIELD)

  // method series

  object invokevirtual extends MethodInsnX(INVOKEVIRTUAL)
  object invokespecial extends MethodInsnX(INVOKESPECIAL)
  object invokestatic extends MethodInsnX(INVOKESTATIC)
  object invokeinterface extends MethodInsnX(INVOKEINTERFACE)
  object invokedynamic

  // misc

  object anew extends TypeInsnX(NEW)
  object initnew extends X {
    def apply(instance: String, owner: String, desc: String): InitNewNode =
      InitNewNode(instance, owner, desc)

    def unapply(insn: Insn): Option[(String, String, String)] = insn match {
      case InitNewNode(instance, owner, desc) => Some((instance, owner, desc))
      case _ => None
    }
  }

  object arraylength extends InsnX(ARRAYLENGTH)
  object athrow extends InsnX(ATHROW)
  object instanceof extends TypeInsnX(INSTANCEOF)
  object monitorenter extends InsnX(MONITORENTER)
  object monitorexit extends InsnX(MONITOREXIT)
}
