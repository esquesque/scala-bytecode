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

import org.objectweb.asm.Type
import scala.bytecode.{Back, MethodInfo}
import scala.bytecode.asm._

class MethodDecl(val modifiers: List[Symbol],
		 val name: String,
		 val desc: String,
		 val argLocals: List[Local],
		 val thrown: List[String],
		 val tryCatches: List[TryCatch],
		 val blocks: List[Block]) extends Exec {
  val body: List[Stmt] = structureBody(blocks.head)
  val returnType: Type = Type.getReturnType(desc)

  var debugIndent: Int = 0
  def debug(str: String) { println((" "* debugIndent) + str) }

  def controlExit(entry: Block): Option[Block] = entry.controlExit

  def structureBody(entry: Block): List[Stmt] = {
    { debug("*structureBody*") }
    struct(entry, entry.controlExit) match {
      case (stmts, Some(trailing)) => stmts ++ structureBody(trailing)
      case (stmts, None) => stmts
    }
  }

  def insertPhiStores(entry: Block, stmts: List[Stmt]): List[Stmt] = {
    val phis = entry.phiLocalsByIndex
    if (phis.isEmpty) stmts
    else {
      val phiStores = (phis map {
	case (v, locals) =>
	  val phi = Phi(locals.toSeq: _*)
	  LocalStore(entry.mklocal(v, Symbol("var_"+ v), phi.desc), phi)
      } ).toList
      stmts.head match {
	case Label(_) =>
	  (stmts.head :: phiStores) ++ stmts.tail
	case _ =>
	  phiStores ++ stmts
      }
    }
  }

  def struct(entry: Block, exit: Option[Block]): (List[Stmt], Option[Block]) = {
    { debugIndent += 2
      debug("*struct* entry=")
      entry.debug(System.out, debugIndent)
      debug("*struct* exit=")
      exit foreach (_.debug(System.out, debugIndent))
    }

    val enteringTryCatches = tryCatches filter (_._1 == entry.bound._1)

    val (stmts, trailing) = if (enteringTryCatches.nonEmpty) {
      structTryCatch(entry,
		     exit.get,
		     enteringTryCatches) match {
	case (stmt, trailing) => (stmt :: Nil, trailing)
      }
    } else {
      val backEdgesIn = entry.edgesIn filter (_._2 == Back)

      backEdgesIn match {
	case Nil => entry match {
	  case BranchBlock(_, cond) => structBranch(entry, exit.get)
	  case _ => (entry.body, None)
	}
        case backEdges =>
	  val loopBlocks = backEdges map (_._1)
	  debug("*struct* loopBlocks=")
	  loopBlocks foreach (_.debug(System.out, debugIndent))
	  structLoop(entry, exit.get, loopBlocks)
      }
    }

    { debugIndent -= 2 }

    (insertPhiStores(entry, stmts), trailing)
  }

  def structTryCatch(entry: Block,
		     exit: Block,
		     tcs: List[TryCatch]): (Stmt, Option[Block]) = {
    { debugIndent += 2 }

    val endIdcs = tcs map (_._2)
    val handlerIdcs = tcs map (_._3)
    val excns = tcs map (_._4)
    println(endIdcs.min)
    val endBlock = (blocks find (_.bound._1 == endIdcs.min)).get

    { debug("*structTryCatch* #"+ entry.ordinal +"-#"+ exit.ordinal)
      debug("*structTryCatch* end=")
      endBlock.debug(System.out, debugIndent)
    }

    val (stmts, tryTrailing) = entry match {
      case BranchBlock(_, cond) if tcs.nonEmpty => structBranch(entry, exit)
      case _ => (entry.body, None)
    }
    val handlerBlocks = handlerIdcs map (idx =>
      (blocks find (_.bound._1 == idx)).get)
    val catchesAndTrailings = (handlerBlocks zip excns) map {
      case (blk, excn) => struct(blk, blk.controlExit) match {
	case (stmts, catchTrailing) =>
	  (Catch(stmts, excn), catchTrailing)
      }
    }

    { debugIndent -= 2 }

    (Try(stmts ++ endBlock.body,
	 (catchesAndTrailings map (_._1)): _*),
     handlerBlocks.last.controlExit)
  }

  def combConds(entry: Block,
		cond: Cond,
		ifBlocks: List[Block],
		falseBlock: Block): Cond = ifBlocks match {
    case Nil => cond.invert
    case BranchBlock(init, cond1) :: rest =>
      val succs = entry.successors
      val next = ifBlocks.head
      val sect = succs intersect next.successors
      //LHA
      if (sect exists (_.ordinal < falseBlock.ordinal))
	combConds(next,
		  if (succs contains falseBlock) Or(cond, cond1)
		  else And(cond.invert, cond1), rest, falseBlock)
      //RHA
      else
	if (succs contains falseBlock)
	  Or(cond, combConds(next, cond1, rest, falseBlock))
	else
	  And(cond.invert, combConds(next, cond1, rest, falseBlock))
    case _ =>
      System.err.println("ERROR")
      System.err.println(" entry=")
      entry.debug(System.err, 2)
      System.err.println(" ifs=")
      ifBlocks foreach {
	_.debug(System.err, 2)
      }
      System.err.println(" false=")
      falseBlock.debug(System.err, 2)
      throw new RuntimeException("unrecognized conditional branch")
  }

  def structCond(entry: Block, exit: Block): (Cond, Block, Block, Block) = {
    val cond = entry match {
      case BranchBlock(_, cond) => cond
      case _ => throw new IllegalArgumentException
    }
    val nord = entry.ordinal
    val xord = exit.ordinal
    val nBlocks = xord - nord
    val subseqBlocks = (((nord + 1) until xord) map blocks).toList
    val ifBlocks = (subseqBlocks takeWhile { blk =>
      if ((BranchBlock unapply blk).isDefined)
	! (blk.immediatelyDominated.nonEmpty &&
	  (blk.immediatelyDominated.init exists (_.successors.isEmpty)))
      else false
    } ) match {
      case Nil => Nil
      case blocks =>
	//termination case for nested ifs:
	//blk1 has outgoing back edges or,
	//blk0 and blk1 do not share the same postdominator (including None
	//which means exit) or,
	//each of blk0.succs are neither immediately dominated by blk1
	//                       nor gain dominance from blk1
	val idx = ((entry :: blocks) zip blocks) indexWhere {
	  case (blk0, blk1) =>
	    (blk1.edgesOut exists (_._2 == Back)) ||
	    blk0.immediatePostdominator != blk1.immediatePostdominator ||
	    (blk0.successors intersect
	      (blk1.immediatelyDominated ++ blk1.dominanceFrontier)).isEmpty
	}
	//if no terminate case, all blocks are contiguous (whatever that means)
	//otherwise just chop off the tail -- it will be nested
	if (idx < 0) blocks else blocks take idx
    }
    val lastIfBlock = ifBlocks.lastOption getOrElse entry
    val falseBlock = blocks(nord + 1 + ifBlocks.length)
    val trueBlock = lastIfBlock.controlExit.get

    (combConds(entry, cond, ifBlocks, falseBlock),
     lastIfBlock, falseBlock, trueBlock)
  }

  /* This function structures a sequence of blocks from entry..exit into an AST
   * of type If representative of those blocks.
   * If may be composed as If(Cond, Then) or If(Cond, Then, Else).
   *
   * This function is experimental.
   */
  def structBranch(entry: Block, exit: Block): (List[Stmt], Option[Block]) = {
    { debugIndent += 2 }

    val (condx, lastIf, thenEntry, thenExit) = structCond(entry, exit)

    {
      debug("*structBranch* lastIf=")
      lastIf.debug(System.out, debugIndent)
      debug("*structBranch* thenEntry=")
      thenEntry.debug(System.out, debugIndent)
      debug("*structBranch* thenExit=")
      thenExit.debug(System.out, debugIndent)
    }

    val thenStruct = struct(thenEntry, Some(thenExit))
    val trailing0 = thenStruct match {
      case (_, None) => lastIf.successors.lastOption
      case (_, Some(t)) => controlExit(t)
    }

    { debug("*structBranch* trailing0=")
      trailing0 foreach (_.debug(System.out, debugIndent))
    }

    val (init, cond) = entry match { case BranchBlock(init, cond) => (init, cond) }

    val nDomRet = entry.immediatelyDominated match {
      case Nil => 0
      case idomd => (idomd.init filter (_.successors.isEmpty)).length
    }

    val ifOnly = trailing0 match {
      case None => true
      case Some(_) if nDomRet > 0 => true
      case Some(t) if t == thenExit && ! (thenEntry.dominanceFrontier contains t) => false
      case Some(t) =>
	(thenEntry.dominanceFrontier intersect t.dominanceFrontier).isEmpty
    }

    val (stmt, trailing) = if (ifOnly) {
      thenStruct match {
	case (stmts, None) =>
	  (If(condx, Then(stmts)), trailing0)
	case (stmts, trailing) =>
	  (If(condx, Then(stmts)), trailing)
      }
    } else {
      val elseEntry = trailing0.get
      val elseExit = controlExit(elseEntry).get

      { debug("*structBranch* elseEntry=")
        elseEntry.debug(System.out, debugIndent)
        debug("*structBranch* elseExit=")
        elseExit.debug(System.out, debugIndent)
      }

      val elseStruct = struct(elseEntry, Some(elseExit))
      val elseEntryJumps = (JumpBlock unapply elseEntry).isDefined
      val elseTrailing = if (elseExit == exit && elseEntryJumps) elseStruct._2
			 else Some(elseExit)

      { debug("*structBranch* elseTrailing=")
        elseTrailing foreach (_.debug(System.out, debugIndent))
        debug("*structBranch* __elseTrailing__=")
        elseStruct._2 foreach println
      }

      (If(condx,
	  Then(thenStruct._1),
	  Else(elseStruct._1)),
       elseTrailing)
    }

    { debugIndent -= 2 }

    if (trailing.isDefined) {
      struct(trailing.get, controlExit(trailing.get)) match {
	case (trailingStmts, nextTrailing) =>
	  { debug("*struct* nextTrailing=")
	    nextTrailing foreach (_.debug(System.out, debugIndent))
	  }
	  if ((entry.dominanceFrontier intersect
		 trailing.get.dominanceFrontier).nonEmpty) {
	    { debug("*struct* CONCAT trailing") }
	    ((init :+ stmt) ++ trailingStmts, nextTrailing)
	  } else {
	    { debug("*struct* PASS trailing") }
	    (init :+ stmt, trailing)
	  }
      }
    } else (init :+ stmt, None)
  }

  def structLoop(entry: Block,
		 exit: Block,
		 backBlocks: List[Block]): (List[Stmt], Option[Block]) = {
    { debugIndent += 2 }

    val (stmts, trailing) = entry match {
      case BranchBlock(init, _) =>
	val (cond, lastBranch, loopEntry, loopExit) = structCond(entry, exit)

	{
	  debug("*structLoop* loopCond="+ cond)
	  debug("*structLoop* lastBranch=")
	  lastBranch.debug(System.out, debugIndent)
	  debug("*structLoop* loopEntry=")
	  loopEntry.debug(System.out, debugIndent)
	  debug("*structLoop* loopExit=")
	  loopExit.debug(System.out, debugIndent)
	}

	val x = (loopEntry.immediateDominator map (idom =>
		  loopEntry.predecessors.length == 1 &&
		  loopEntry.predecessors(0) == idom) getOrElse false) &&
		(loopEntry.immediatePostdominator map (ipdom =>
		  loopEntry.successors.length == 1 &&
		  loopEntry.successors(0) == ipdom) getOrElse false)

	val (body, trailing) =
	  if ((backBlocks contains loopEntry) && !x)
	    structLoop(loopEntry, exit, Nil)
	  else
	    struct(loopEntry, Some(loopExit))
	(init :+ While(cond, body), Some(loopExit))
      case _ =>
	//do
	throw new RuntimeException
    }

    { debugIndent -= 2 }

    (stmts, trailing)
  }

  def out(ps: java.io.PrintStream, indent: Int) {
    ps append " "* indent
    if (modifiers.nonEmpty) {
      ps append (modifiers map (_.name) mkString " ")
      ps append " "
    }
    ps append javaTypeString(returnType)
    ps append ' '
    ps append name
    ps append '('
    ps append (argLocals map (_ show false) mkString ", ")
    ps append ')'
    thrown match {
      case x if x.nonEmpty => ps append x.mkString(" throws ", ", ", "")
      case _ =>
    }
    if (modifiers contains 'abstract) {
      ps append "\n"
      return
    }
    ps append " {\n"
    body foreach { stmt =>
      stmt.out(ps, indent + 2)
      ps append '\n'
    }
    ps append " "* indent
    ps append "}\n"
    ps.flush
  }
}

object MethodDecl {
  def apply(method: MethodInfo): MethodDecl = {
    val analyzer = method.cfgAnalyzer(MethodInfo.sourceInterpreter)
    val frames = analyzer.analyze(method.owner.name, method.node)
    val cfg = method.cfg
    val blocks = cfg mkblocks frames
    val entry = blocks.head
    val argLocals = method.arguments map {
      case (v, desc) => entry.mklocal(v, Symbol("arg_"+ v), desc)
    }
    new MethodDecl(method.modifiers, method.name, method.desc,
		   argLocals, method.thrown, method.tryCatches, blocks)
  }
}
