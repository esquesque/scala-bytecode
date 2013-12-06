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

import java.io.PrintStream

import asm._

object ast {
  trait AST

  def descOf(value: Any): String = value match {
    case _: Int => "I"
    case _: Long => "J"
    case _: Float => "F"
    case _: Double => "D"
    case _: Class[_] => "Ljava/lang/Class;"
    case _: String => "Ljava/lang/String;"
    case null => "Ljava/lang/Object;"
    case _ => throw new RuntimeException("descOf("+ value +")")
  }

  trait Expr extends AST {
    def desc: String
    def show(cap: Boolean): String
  }

  case class Local(index: Int, id: Symbol, desc: String) extends Expr {
    def show(cap: Boolean) = desc +"_"+ id.name
  }

  case class Push[V](value: V) extends Expr {
    val desc = descOf(value)
    def show(cap: Boolean) = value match {
      case null => "null"
      case str: String => "\""+ str +"\""
      case _ => value.toString
    }
  }

  val zero = Push(0)

  case class Math1(desc: String, operator: String, expr: Expr) extends Expr {
    def show(cap: Boolean) = (operator +" "+ (expr show true)) match {
      case str if cap => "("+ str +")"; case str => str
    }
  }

  case class Math2(desc: String,
		   left: Expr, operator: String, right: Expr) extends Expr {
    def show(cap: Boolean) =
      ((left show true) +" "+ operator +" "+ (right show true)) match {
	case str if cap => "("+ str +")"; case str => str
      }
  }

  case class Inc[V](local: Local, amount: V) extends Expr {
    val desc = local.desc
    def show(cap: Boolean) = (local show false) +" += "+ amount
  }

  case class Cast(from: Option[String], to: String, expr: Expr) extends Expr {
    val desc = to
    def show(cap: Boolean) = ("("+ to +") "+ (expr show true)) match {
      case str if cap => "("+ str +")"; case str => str
    }
  }

  trait cond
  trait ShCir extends cond {
    val invert: (Expr, Expr) => cond  = (_, _) => this
  }

  abstract class Cond(val string: String, invert: (Expr, Expr) => cond)
	   extends Expr with cond {
    def left: Expr
    def right: Expr

    val desc = "Z"
    lazy val inverse = invert(left, right)

    def show(cap: Boolean) =
      left.show(true) +
      " "+ string +" "+
      right.show(true) match {
	case str if cap => "("+ str +")"
	case str => str
      }
  }

  case class Cmp(left: Expr, right: Expr) extends Expr {
    val desc = "I"

    def show(cap: Boolean) =
      (left.show(true) + " cmp "+ right.show(true)) match {
	case str if cap => "("+ str +")"
	case str => str
      }
  }

  case class ShCirAnd(left: Cond, right: Cond) extends Cond("&&", null) with ShCir
  case class ShCirCOr(left: Cond, right: Cond) extends Cond("||", null) with ShCir

  case class Eq(left: Expr, right: Expr) extends Cond("==", Ne(_, _))
  case class Ne(left: Expr, right: Expr) extends Cond("!=", Eq(_, _))
  case class Lt(left: Expr, right: Expr) extends Cond("<", Ge(_, _))
  case class Ge(left: Expr, right: Expr) extends Cond(">=", Lt(_, _))
  case class Gt(left: Expr, right: Expr) extends Cond(">", Le(_, _))
  case class Le(left: Expr, right: Expr) extends Cond("<=", Gt(_, _))

  case class Field(owner: String, name: String, desc: String,
		   obj: Option[Expr]) extends Expr {
    def show(cap: Boolean) =
      (obj map (_ show true ) getOrElse owner) +"/"+ name
  }

  case class Method(owner: String, name: String, invokeDesc: String,
		    obj: Option[Expr], args: List[Expr]) extends Expr {
    val desc = invokeDesc.substring(invokeDesc.indexOf(')') + 1)
    def show(cap: Boolean) =
      (obj map (_ show true) getOrElse owner) +"/"+ name +
      (args map (_ show false) mkString ("(", ", ", ")"))
  }

  case class New(instance: String) extends Expr {
    val desc = 'L'+ instance +';'
    def show(cap: Boolean) = ("new "+ instance) match {
      case str if cap => '('+ str +')'; case str => str
    }
  }

  case class InitNew(instance: String, owner: String, invokeDesc: String,
		     args: List[Expr]) extends Expr {
    val desc = invokeDesc.substring(invokeDesc.indexOf(')') + 1)
    def show(cap: Boolean) =
      ("new "+ instance +
       (args map (_ show false) mkString ("(", ", ", ")"))) match {
	 case str if cap => '('+ str +')'; case str => str
       }
  }

  case object Thrown extends Expr {
    val desc = "Ljava/lang/Throwable;"//TODO
    def show(cap: Boolean) = "!!!"
  }

/******************************************************************************/

  trait Stmt extends AST {
    def out(ps: PrintStream, indent: Int)
    def out() { out(System.out, 0) }
  }
  trait Exec extends Stmt { def body: List[Stmt] }

  case class Void(expr: Expr) extends Stmt {
    def out(ps: PrintStream, indent: Int) {
      ps append " "* indent
      ps append (expr show false)
      ps append ';'
      ps.flush
    }
  }

  case class Store(local: Local, expr: Expr) extends Stmt {
    def out(ps: PrintStream, indent: Int) {
      ps append " "* indent
      ps append (local show false)
      ps append " = "
      ps append (expr show false)
      ps append ';'
      ps.flush
    }
  }

  case class If(cond: Cond, stmt: Stmt) extends Stmt {
    def out(ps: PrintStream, indent: Int) {
      ps append " "* indent
      ps append "if ("
      ps append (cond show false)
      ps append ") "
      stmt.out(ps, if (stmt.isInstanceOf[Goto]) 0 else indent + 2)
      ps.flush
    }
  }

  //case class Then
  //case class ThenElse

  //case class While

  case class Label(symbol: Symbol) extends Stmt {
    def out(ps: PrintStream, indent: Int) {
      ps append " "* indent
      ps append ':'
      ps append symbol.name
      ps append ';'
      ps.flush
    }
  }

  case class Goto(labelId: Symbol, getblk: () => Option[Block]) extends Stmt {
    lazy val target: Option[Block] = getblk()

    def out(ps: PrintStream, indent: Int) {
      ps append " "* indent
      ps append "goto "
      ps append labelId.name
      ps append ";//"
      ps append (target map (_.bound.toString) getOrElse "nowhere!")
      ps.flush
    }
  }

  case class Return(value: Option[Expr]) extends Stmt {
    def out(ps: PrintStream, indent: Int) {
      ps append " "* indent
      value match {
	case Some(expr) =>
	  ps append "return "
	  ps append (expr show false)
	  ps append ';'
	case None => ps append "return;"
      }
      ps.flush
    }
  }

  class ClassDecl(val modifiers: List[Symbol],
		  val name: String,
		  val superName: String,
		  val interfaces: List[String],
		  val body: List[Stmt]) extends Exec {
    def out(ps: PrintStream, indent: Int) {
      val interface = modifiers contains 'interface
      ps append " "* indent
      if (modifiers.nonEmpty) {
	ps append (modifiers filter ('interface != _) map (_.name)).mkString(" ")
	ps append " "
      }
      ps append (if (interface) "interface " else "class ")
      ps append name
      ps append " extends "
      ps append superName
      interfaces match {
	case x if x.nonEmpty =>
	  ps append x.mkString(if (interface) ", " else " implements ", ", ", "")
	case _ =>
      }
      ps append " {\n"
      body foreach { _.out(ps, indent + 2) }
      ps append "}\n"
      ps.flush
    }
  }

  class FieldDecl(val modifiers: List[Symbol],
		  val name: String,
		  val desc: String,
		  val init: Option[Any]) extends Stmt {
    def out(ps: PrintStream, indent: Int) { }
  }

  import org.objectweb.asm.tree.{AbstractInsnNode => Insn}
  import org.objectweb.asm.tree.analysis.{Frame, SourceValue}
  class Block(val ordinal: Int,
	      val bound: (Int, Int),
	      val info: MethodInfo,
	      val frames: Array[Frame]) extends Exec {
    lazy val dominator: Option[Block] =
      info.cfg.dominators(ordinal) match {
	case self if self == bound => None
	case dom => Some(info.cfg.blocks(info.cfg.bounds indexOf dom))
      }

    /* excludes self-domination by entry block */
    lazy val dominated: List[Block] =
      (info.cfg.dominatedBy(ordinal)).toList match {
	case self :: rest if self == ordinal => rest map info.cfg.blocks
	case subs => subs map info.cfg.blocks
      }

    lazy val dominanceFrontier: List[Block] =
      (info.cfg.dominanceFrontiers(ordinal)).toList map info.cfg.blocks

    /* check ordinal and bound */
    def ordinallyPrecedes(subseq: Block*): Boolean = subseq.headOption match {
      case None => true
      case Some(block) =>
	ordinal + 1 == block.ordinal && bound._2 == block.bound._1 &&
	block.ordinallyPrecedes(subseq.tail: _*)
    }

    private val locals: Array[Local] = new Array(info.node.maxLocals)

    private def local(index: Int, id: Symbol, desc: String): Local = {
      val loc = Local(index, id, desc)
      locals(index) = loc
      loc
    }

    def loadLocal(v: Int): Local = locals(v) match {
      case null if v == 0 && ! (info is 'static) =>
	local(0, 'this, info.owner.desc)
      case null => info.arguments.find(_._1 == v) match {
	case Some((_, desc)) => local(v, Symbol("arg_"+ v), desc)
	case None => throw new RuntimeException("var_"+ v +" is unavailable")
      }
      case loc => loc
    }

    def storeLocal(v: Int, expr: Expr): Store = locals(v) match {
      case null => info.arguments.find(_._1 == v) match {
	case Some((_, desc)) =>
	  Store(local(v, Symbol("arg_"+ v), expr.desc), expr)
	case None =>
	  Store(local(v, Symbol("var_"+ v), expr.desc), expr)
      }
      case loc => Store(loc, expr)
    }

    def labelId(insn: Insn): Symbol = Symbol(insnName(insn))

    //private def getblk(insn: Insn): () => Option[Block] = 

    private def mkgoto(lbl: Insn): Goto = Goto(labelId(lbl), { () =>
      val x = info.instructions.indexOf(lbl)
      info.cfg.blocks.find(_.bound._1 == x)
    } )

    private def mkcond1(cond: (Expr, Expr) => Cond, expr: Expr): Cond = expr match {
      case Cmp(left, right) => cond(left, right)
      case _ => cond(expr, zero)
    }

    import java.util.{Set => set}
    import scala.collection.JavaConversions._
    def mkexpr(sourceValue: SourceValue): Expr =
      sourceValue.insns.asInstanceOf[set[Insn]].toList match {
	case Nil => Thrown
	case insn :: Nil =>
	  val i = bound._1 +
	    (info.instructions.slice(bound._1, bound._2) indexOf insn)
	  val frame = frames(i)
	  val stack = for (j <- (frames(i + 1).getStackSize - 1 until frame.getStackSize))
		      yield frame.getStack(j).asInstanceOf[SourceValue]
	  val f: Int => Expr = { j => mkexpr(stack(j)) }
	  lazy val args = stack.toList map mkexpr
	  insn match {
	    case push(any) => Push(any)
	    case load(v, _) => loadLocal(v)
	    //case array.load(desc) =>
	    case math(desc, operator, valency) => valency match {
	      case 1 => Math1(desc, operator, f(0))
	      case 2 => Math2(desc, f(0), operator, f(1))
	    }
	    case cast(from, to) => Cast(from, to, f(0))
	    case cmp(_) => Cmp(f(0), f(1))
	    case getstatic(own, name, desc) => Field(own, name, desc, None)
	    //case getfield
	    case initnew(inst, own, desc) => InitNew(inst, own, desc, args)
	    case invokevirtual(own, name, desc) =>
	      Method(own, name, desc, args.headOption, args.tail)
	    //case invokespecial
	    case invokestatic(own, name, desc) =>
	      Method(own, name, desc, None, args)
	    //case invokeinterface
	    //case invokedynamic
	    case anew(inst) => New(inst)
	    //case array.alloc()
	    //case array.length()
	    //case instanceof
	    case _ => throw new RuntimeException("mkexpr:"+ insnName(insn))
	  }
	case insn0 :: insn1 :: _ => throw new RuntimeException
      }

    def mkstmt(preZero: Int): Stmt = {
      val frame = frames(preZero)
      val stack = for (i <- (0 until frame.getStackSize))
		  yield frame.getStack(i).asInstanceOf[SourceValue]
      val f: Int => Expr = { i => mkexpr(stack(i)) }
      lazy val args = ((0 until frame.getStackSize) map f).toList
      val insn = info.instructions(preZero)
      val stmt = insn match {
	case store(v, _) => storeLocal(v, f(0))
	/*case array.store(_) => Array.Store
	case pop() => Void
	case pop2() => //Void x two??? */
	case iinc(v, n) => Void(Inc(loadLocal(v), n))
	case label() => Label(labelId(insn))
	case goto(lbl) => mkgoto(lbl)
	case ifeq(lbl) => If(mkcond1(Eq, f(0)), mkgoto(lbl))
	case ifne(lbl) => If(mkcond1(Ne, f(0)), mkgoto(lbl))
	case iflt(lbl) => If(mkcond1(Lt, f(0)), mkgoto(lbl))
	case ifge(lbl) => If(mkcond1(Ge, f(0)), mkgoto(lbl))
	case ifgt(lbl) => If(mkcond1(Gt, f(0)), mkgoto(lbl))
	case ifle(lbl) => If(mkcond1(Le, f(0)), mkgoto(lbl))
	case if_icmpeq(lbl) => If(Eq(f(0), f(1)), mkgoto(lbl))
	case if_icmpne(lbl) => If(Ne(f(0), f(1)), mkgoto(lbl))
	case if_icmplt(lbl) => If(Lt(f(0), f(1)), mkgoto(lbl))
	case if_icmpge(lbl) => If(Ge(f(0), f(1)), mkgoto(lbl))
	case if_icmpgt(lbl) => If(Gt(f(0), f(1)), mkgoto(lbl))
	case if_icmple(lbl) => If(Le(f(0), f(1)), mkgoto(lbl))
	//case putstatic
        //case putfield
	case invokevirtual(own, name, desc) =>
	  Void(Method(own, name, desc, args.headOption, args.tail))
	case invokespecial(own, name, desc) =>
	  Void(name match {
	    case "<init>" => args.head match {
	      case New(inst) => InitNew(inst, own, desc, args.tail)
	      case _ => Method(own, name, desc, args.headOption, args.tail)
	    }
	  } )
	case invokestatic(own, name, desc) =>
	  Void(Method(own, name, desc, None, args))
	//case invokeinterface
	//case invokedynamic
	case vreturn() => Return(None)
	case _ => throw new RuntimeException("mkstmt:"+ insnName(insn))
      }
      stmt
    }

    lazy val body = {
      val range = (bound._1 until bound._2) ++
		  (info.cfg successors bound match {
	case (x, _) :: _ => x :: Nil; case _ => 0 :: Nil
      } )
      val stackPreZeros = for (i <- 0 until range.length - 1
			       if frames(range(i + 1)).getStackSize == 0)
			  yield range(i)
      (stackPreZeros map mkstmt).toList
    }

    def out(ps: PrintStream, indent: Int) {
      debug(ps, indent)
      for (stmt <- body) {
	stmt.out(ps, indent)
	ps append '\n'
      }
    }

    def debug(ps: PrintStream, indent: Int) {
      def str(b: (Int, Int)): String = b._1 +".."+ b._2
      ps append " "* indent
      ps append "//block "
      ps append str(bound)
      ps append " "
      ps append info.cfg.predecessors(bound).map(str).mkString("[", ", ", "]")
      ps append "->; ->"
      ps append info.cfg.successors(bound).map(str).mkString("[", ", ", "]")
      ps append '\n'
    }
  }

  class MethodDecl(val modifiers: List[Symbol],
		   val name: String,
		   val desc: String,
		   val thrown: List[String],
		   val body: List[Stmt]) extends Exec {
    def out(ps: PrintStream, indent: Int) {
      ps append " "* indent
      if (modifiers.nonEmpty) {
	ps append modifiers.map(_.name).mkString(" ")
	ps append " "
      }
      ps append name
      ps append desc
      thrown match {
	case x if x.nonEmpty => ps append x.mkString(" throws ", ", ", "")
	case _ =>
      }
      if (modifiers contains 'abstract) {
	ps append "\n"
	return
      }
      ps append " {\n"
      body foreach { _.out(ps, indent + 2) }
      ps append " "* indent
      ps append "}\n"
      ps.flush
    }
  }

  object ClassDecl {
    def apply(info: ClassInfo): ClassDecl =
      new ClassDecl(info.modifiers, info.name, info.superName, info.interfaces,
		    (info.fields map FieldDecl.apply) ++
		    (info.methods map MethodDecl.apply))
  }

  object FieldDecl {
    def apply(info: FieldInfo): FieldDecl =
      new FieldDecl(info.modifiers, info.name, info.desc, info.init)
  }

  object MethodDecl {
    def apply(info: MethodInfo): MethodDecl =
      new MethodDecl(info.modifiers,
		     info.name,
		     info.desc,
		     info.thrown,
		     info.cfg.blocks)
  }
}
