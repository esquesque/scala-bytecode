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

package scala.bytecode.ast

import org.objectweb.asm.tree.{AbstractInsnNode => Insn}
import org.objectweb.asm.tree.analysis.{Frame, SourceValue}

import scala.bytecode.{MethodInfo, ControlFlowGraph}
import scala.bytecode.asm._

import scala.collection.immutable.HashSet

class Block(val ordinal: Int,
	    val bound: (Int, Int),
	    val info: MethodInfo,
	    val frames: Array[Frame],
	    val cfg: ControlFlowGraph,
	    getblocks: () => List[Block]) extends Exec {
  private lazy val blocks = getblocks()

  lazy val predecessors: List[Block] =
    (cfg predecessors bound) map (pred => blocks(cfg.bounds indexOf pred))

  lazy val successors: List[Block] =
    (cfg successors bound) map (succ => blocks(cfg.bounds indexOf succ))

  def span(to: Block): Set[(Block, Block)] = {
    val succs = successors filter (_.ordinal <= to.ordinal)
    val edges =
      if (succs.isEmpty) HashSet.empty
      else succs map (_ span to) reduce (_ ++ _)
    edges ++ HashSet(succs map (succ => this -> succ): _*)
  }

  lazy val dominator: Option[Block] =
    cfg.dominators(ordinal) match {
      case self if self == bound => None
      case dom => Some(blocks(cfg.bounds indexOf dom))
    }

  /* excludes self-domination by entry block */
  lazy val dominated: List[Block] =
    cfg.dominatedBy(ordinal).toList match {
      case self :: rest if self == ordinal => rest map blocks
      case subs => subs map blocks
    }

  lazy val dominates: Boolean = dominated.length > 0

  lazy val dominanceFrontier: List[Block] =
    info.cfg.dominanceFrontiers(ordinal).toList map blocks

  lazy val dominanceLost: List[Block] =
    info.cfg.dominanceLostBy(ordinal).toList map blocks

  /* check ordinal and bound */
  def ordinallyPrecedes(subseq: Block*): Boolean = subseq.headOption match {
    case None => true
    case Some(block) =>
      ordinal + 1 == block.ordinal && bound._2 == block.bound._1 &&
      block.ordinallyPrecedes(subseq.tail: _*)
  }

  private val locals: Array[Local] = new Array(info.node.maxLocals)

  def local(v: Int, id: Symbol, desc: String): Local = {
    val loc = Local(v, id, desc)
    locals(v) = loc
    loc
  }

  //TODO
  def mergePredLocal(v: Int): Option[Local] = {
    (predecessors map (_.loadLocal(v))).headOption
  }

  def loadLocal(v: Int): Local = locals(v) match {
    case null if v == 0 && ! (info is 'static) =>
      local(0, 'this, info.owner.desc)
    case null => info.arguments.find(_._1 == v) match {
      case Some((_, desc)) => local(v, Symbol("arg_"+ v), desc)
      case None => mergePredLocal(v) match {
	case Some(loc) =>
	  locals(v) = loc
	  loc
	case None =>
	  throw new RuntimeException("var_"+ v +" is unavailable")
      }
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

  private def mkgoto(lbl: Insn): Goto = Goto(labelId(lbl), {
    val x = info.instructions.indexOf(lbl)
    cfg.bounds indexWhere { case (y, _) => y == x }
  } )

  private def mkcond1(cond: (Expr, Expr) => Cond, expr: Expr): Cond = expr match {
    case Cmp(left, right) => cond(left, right)
    case _ => cond(expr, Zero)
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
	  case math(desc, operator, 1) => UnaryMath(desc, operator, f(0))
	  case math(desc, operator, 2) => BinaryMath(desc, f(0), operator, f(1))
	  case cast(from, to) => Cast(from, to, f(0))
	  case cmp(_) => Cmp(f(0), f(1))
	  case getstatic(own, name, desc) => Field(own, name, desc, None)
	  case getfield(own, name, desc) => Field(own, name, desc, Some(f(0)))
	  case initnew(inst, own, desc) => InitNew(inst, own, desc, args)
	  case invokevirtual(own, name, desc) =>
	    Method(own, name, desc, args.headOption, args.tail)
	  //case invokespecial
	  case invokestatic(own, name, desc) =>
	    Method(own, name, desc, None, args)
	  //case invokeinterface
	  //case invokedynamic
	  case anew(inst) => New(inst)
	  case array.alloc(desc) => ArrayAlloc(desc, f(0))
	  case array.length() => ArrayLength(f(0))
	  //case instanceof
	  case _ => throw new RuntimeException("mkexpr:"+ insnName(insn))
	}
      case insn0 :: insn1 :: _ =>
	println(insnString(insn0))
	println(insnString(insn1))
	throw new RuntimeException("wooo ooops")
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
      case array.store(_) => ArrayStore(f(0), f(1), f(2))
      case pop() => Void(f(0))
      //case pop2() => Void
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
      case if_acmpeq(lbl) => If(Eq(f(0), f(1)), mkgoto(lbl))
      case if_acmpne(lbl) => If(Ne(f(0), f(1)), mkgoto(lbl))
      case ifnull(lbl) => If(Eq(f(0), Null), mkgoto(lbl))
      case ifnonnull(lbl) => If(Ne(f(0), Null), mkgoto(lbl))
      case putstatic(own, name, desc) =>
	Store(Field(own, name, desc, None), f(0))
      case putfield(own, name, desc) =>
	Store(Field(own, name, desc, Some(f(0))), f(1))
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
      case ireturn() => Return(Some(f(0)))
      case lreturn() => Return(Some(f(0)))
      case freturn() => Return(Some(f(0)))
      case dreturn() => Return(Some(f(0)))
      case areturn() => Return(Some(f(0)))
      case vreturn() => Return(None)
      //case monitorenter() =>
      //case monitorexit() =>
      case _ => throw new RuntimeException("mkstmt:"+ insnName(insn))
    }
    stmt
  }

  lazy val body = {
    val range = (bound._1 until bound._2) ++
    (cfg successors bound match {
      case (x, _) :: _ => x :: Nil; case _ => 0 :: Nil
    } )
    val stackPreZeros = for (i <- 0 until range.length - 1
			     if frames(range(i + 1)).getStackSize == 0)
			yield range(i)
    (stackPreZeros map mkstmt).toList
  }

  def out(ps: java.io.PrintStream, indent: Int) {
    debug(ps, indent)
    for (stmt <- body) {
      stmt.out(ps, indent)
      ps append '\n'
    }
  }

  def debug(ps: java.io.PrintStream, indent: Int) {
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

  override def toString = {
    val insns = info.instructions
    "[#"+ ordinal +"; "+ bound._1 +".."+ bound._2 +"; "+ (bound match {
      case (beg, end) if beg == end - 1 =>
	insnString(insns(beg))
      case (beg, end) if beg == end - 2 =>
	insnString(insns(beg)) +"; "+ insnString(insns(end - 1))
      case (beg, end) =>
	insnString(insns(beg)) +"..."+ insnString(insns(end - 1))
    } ) +']'
  }

  override def equals(any: Any): Boolean = any match {
    case block: Block => block.ordinal == ordinal && (block.bound equals bound)
    case _ => false
  }
}
