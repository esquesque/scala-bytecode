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

import scala.bytecode.{EdgeKind, MethodInfo, ControlFlowGraph}
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

  lazy val edgesIn: List[(Block, EdgeKind)] = predecessors map (pred =>
    (pred, cfg.dfst.edgeKinds(pred.ordinal -> ordinal)))
  lazy val edgesOut: List[(Block, EdgeKind)] = successors map (succ =>
    (succ, cfg.dfst.edgeKinds(ordinal -> succ.ordinal)))

  /* DFST parent.
   */
  lazy val parent: Option[Block] =
    cfg.dfst.parents(ordinal) map (n => blocks(n.n))

  /* DFST children.
   */
  lazy val children: List[Block] =
    cfg.dfst.children(ordinal).toList map (n => blocks(n.n))

  /* Returns None whenever this block has no incoming edges.
   */
  lazy val immediateDominator: Option[Block] =
    (cfg immediateDominators ordinal) match {
      case Some(self) if self == bound => None
      case Some(idomBounds) => Some(blocks(cfg.bounds indexOf idomBounds))
      case None => None
    }

  /* Excludes self-domination by entry block. */
  lazy val immediatelyDominated: List[Block] =
    (cfg immediatelyDominatedBy ordinal).toList match {
      case self :: idomdOrds if self == ordinal => idomdOrds map blocks
      case idomdOrds => idomdOrds map blocks
    }

  lazy val dominators: List[Block] = immediateDominator map (idom =>
    idom.dominators :+ idom) getOrElse Nil

  //

  private lazy val seRevCFG: ControlFlowGraph = cfg.singleExitReverse
  private lazy val seRevSize: Int = seRevCFG.bounds.length
  private lazy val seRevOrd: Int = seRevSize - 1 - ordinal

  /* Excludes domination by dummy exit block (-1, -1). */
  lazy val immediatePostdominator: Option[Block] =
    (seRevCFG immediateDominators seRevOrd) match {
      case Some((-1, -1)) => None
      case Some(ipdomBounds) => Some(blocks(cfg.bounds indexOf ipdomBounds))
      case None => None
    }

  lazy val immediatelyPostdominated: List[Block] =
    (seRevCFG immediatelyDominatedBy seRevOrd).toList.reverse map (ipdomdOrd =>
      blocks(seRevSize - 1 - ipdomdOrd))

  lazy val postdominators: List[Block] = immediatePostdominator map (ipdom =>
    ipdom.postdominators :+ ipdom) getOrElse Nil

  def dominates(block: Block): Boolean =
    if (block equals this) true else block.dominators contains this
  def strictlyDominates(block: Block): Boolean =
    if (block equals this) false else block.dominators contains this
  def immediatelyDominates(block: Block): Boolean =
    block.immediateDominator map (_ equals this) getOrElse false
  def postdominates(block: Block): Boolean =
    if (block == this) true else block.postdominators contains this
  def strictlyPostdominates(block: Block): Boolean =
    if (block == this) false else block.postdominators contains this
  def immediatelyPostdominates(block: Block): Boolean =
    block.immediatePostdominator map (_ equals this) getOrElse false

  lazy val dominanceFrontier: List[Block] =
    (info.cfg dominanceFrontiers ordinal).toList map blocks

  lazy val controlExit: Option[Block] =
    immediatePostdominator match {
      case None => immediatelyDominated.lastOption
      case ipdom => ipdom
    }

  //SSA

  private val locals: Array[Local] = new Array(info.node.maxLocals)

  def liveLocals: List[Local] = locals.toList filter (_ != null)

  def mklocal(v: Int, id: Symbol, desc: String): Local = {
    val local = Local(v, id, desc); locals(v) = local; local
  }

  def mklocal(v: Int, desc: String): Local =
    mklocal(v, info.uniqueLocalId("var_", v), desc)

  def phiLocalsByIndex: Map[Int, Set[Local]] = immediatelyPostdominated match {
    case Nil => Map.empty
    case ipdomd =>
      (ipdomd map (_.liveLocals)).toSet.flatten groupBy (_.index) filter {
	case (v, phiLocals) => phiLocals.size > 1//no
      }
  }

  // check-this -> check-arg -> check-phi -> check-idom
  def loadLocal(v: Int): Local = locals(v) match {
    case null if v == 0 && ! (info is 'static) =>
      mklocal(0, 'this, info.owner.desc)
    case null =>
      (info.arguments find (_._1 == v)) match {
	case Some((_, desc)) => mklocal(v, Symbol("arg_"+ v), desc)
	case None =>
	  (phiLocalsByIndex get v) match {
	    case Some(phiLocals) =>
	      mklocal(v, Symbol("var_"+ v),
		      info.cxt commonDesc (phiLocals map (_.desc)).toSeq)
	    case None =>
	      (immediateDominator map (_ loadLocal v)) match {
		case Some(idomLocal) => idomLocal
		case None =>
		  System.err.println("!!! no local at index "+ v +" !!!")
		  debug(System.err, 0)
		  throw new RuntimeException
	      }
	  }
      }
    case local => local
  }

  def storeLocal(v: Int, expr: Expr): LocalStore = locals(v) match {
    case null =>
      val local = (info.arguments find (_._1 == v)) match {
	case Some((_, desc)) => mklocal(v, Symbol("arg_"+ v), desc)
	case None => mklocal(v, expr.desc)
      }
      locals(v) = local
      LocalStore(local, expr)
    case local =>
      LocalStore(local, expr)
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
	  case array.load(desc) => ArrayLoad(f(0), f(1))
	  case array.alloc(desc) => ArrayAlloc(desc, f(0))
	  case math(desc, operator, 1) => UnaryMath(desc, operator, f(0))
	  case math(desc, operator, 2) => BinaryMath(desc, f(0), operator, f(1))
	  case cast(from, to) => Cast(from, to, f(0))
	  case cmp(_) => Cmp(f(0), f(1))
	  case getstatic(own, name, desc) => Field(own, name, desc, None)
	  case getfield(own, name, desc) => Field(own, name, desc, Some(f(0)))
	  case initnew(inst, own, desc) => InitNew(inst, own, desc, args)
	  case invokevirtual(own, name, desc) =>
	    Method(own, name, desc, args.headOption, args.tail)
	  case invokespecial(own, name, desc) =>
	    args.head match {
	      case New(inst) if name equals "<init>" =>
		InitNew(inst, own, desc, args.tail)
	      case _ => Method(own, name, desc, args.headOption, args.tail)
	    }
	  case invokestatic(own, name, desc) =>
	    Method(own, name, desc, None, args)
	  case invokeinterface(own, name, desc) =>
	    Method(own, name, desc, args.headOption, args.tail)
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
      case goto(lbl) => mkgoto(lbl)
      //case jsr
      //case ret
      case tableswitch(min, max, default, labels) =>
	OrdSwitch[Int](f(0),
		       (min to max).toList zip (
			 (labels map mkgoto) map (_.targetOrd)),
		       mkgoto(default).targetOrd)
      case lookupswitch(default, keys, labels) =>
	OrdSwitch[Int](f(0),
		       keys zip ((labels map mkgoto) map (_.targetOrd)),
		       mkgoto(default).targetOrd)
      case ireturn() => Return(Some(f(0)))
      case lreturn() => Return(Some(f(0)))
      case freturn() => Return(Some(f(0)))
      case dreturn() => Return(Some(f(0)))
      case areturn() => Return(Some(f(0)))
      case vreturn() => Return(None)
      case putstatic(owner, name, desc) =>
	RefStore(Field(owner, name, desc, None), f(0))
      case putfield(owner, name, desc) =>
	RefStore(Field(owner, name, desc, Some(f(0))), f(1))
      case invokevirtual(owner, name, desc) =>
	Void(Method(owner, name, desc, args.headOption, args.tail))
      case invokespecial(owner, name, desc) =>
	Void(args.head match {
	  case New(inst) if name equals "<init>" =>
	    InitNew(inst, owner, desc, args.tail)
	  case _ => Method(owner, name, desc, args.headOption, args.tail)
	} )
      case invokestatic(owner, name, desc) =>
	Void(Method(owner, name, desc, None, args))
      case invokeinterface(owner, name, desc) =>
	Void(Method(owner, name, desc, args.headOption, args.tail))
      //case invokedynamic
      case athrow() => Throw(f(0))
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
    (for (i <- 0 until range.length - 1
	  if frames(range(i + 1)).getStackSize == 0)
     yield mkstmt(range(i))).toList
  }

  def out(ps: java.io.PrintStream, indent: Int) {
    debug(ps, indent)
    for (stmt <- body) {
      stmt.out(ps, indent)
      ps append '\n'
    }
  }

  def debug(ps: java.io.PrintStream, indent: Int) {
    def ordlistr(pre: String, bs: List[Block], post: String): String =
      if (bs.isEmpty) pre +"[]"+ post
      else bs.map(_.ordinal).mkString(pre +"[#", ", #", "]"+ post)
    ps append " "* indent
    ps append "//"
    ps append toString
    ps append ordlistr(" ", predecessors, "-->*-->")
    //ps append ordlistr("", successors, " eks=")
    ps append ordlistr("", successors, " parent=")
/*    val dfst = cfg.dfst
    ps append successors.map(s =>
      dfst.edgeKinds(ordinal -> s.ordinal)).mkString("[", ", ", "] ")
    ps append (dfst.pre(ordinal).n +"/"+ dfst.post(ordinal).n +" parent=")*/
    parent foreach { p => ps append '#'; ps append (String valueOf p.ordinal) }
    /*ps append ordlistr(" children=", children, " idom=")*/
    ps append " idom="
    immediateDominator foreach {
      d => ps append '#'; ps append (String valueOf d.ordinal) }
    ps append " ipdom="
    immediatePostdominator foreach {
      pd => ps append '#'; ps append (String valueOf pd.ordinal) }
    ps append ordlistr(" idomd=", immediatelyDominated, "")
    ps append ordlistr(" ipdomd=", immediatelyPostdominated, "")
    ps append ordlistr(" df=", dominanceFrontier, "")
    ps append '\n'
  }

  override def toString = {
    val insns = info.instructions
    "(#"+ ordinal +"; "+
    bound._1 +".."+ bound._2 +"; "+
    (bound match {
      case (beg, end) if beg == end - 1 =>
	insnString(insns(beg))
      case (beg, end) if beg == end - 2 =>
	insnString(insns(beg)) +"; "+ insnString(insns(end - 1))
      case (beg, end) =>
	insnString(insns(beg)) +"..."+ insnString(insns(end - 1))
    } ) +')'
  }

  override def equals(any: Any): Boolean = any match {
    case block: Block => (block.bound equals bound)
    case _ => false
  }
}
