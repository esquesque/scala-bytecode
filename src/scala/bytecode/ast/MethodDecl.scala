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
import scala.bytecode.MethodInfo
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
    val exit = entry.controlExit
    struct(entry, exit) match {
      case (stmts, Some(next)) => stmts ++ structureBody(next)
      case (stmts, None) => stmts
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

    val ret = entry match {
      case _ if enteringTryCatches.nonEmpty =>
        (structTryCatch(entry,
			exit.get,
			enteringTryCatches) :: Nil, None)
      case IfBlock(_, init, cond) =>
	structIf(entry, exit.get, cond) match {
	  case (stmt, None) => (init :+ stmt, None)
	  case (stmt, Some(trailing)) =>
	    { debug("*struct* trailing=")
	      trailing.debug(System.out, debugIndent)
	    }
	    struct(trailing, controlExit(trailing)) match {
	      case (trailingStmts, nextTrailing) =>
		{ debug("*struct* nextTrailing=")
		  nextTrailing foreach (_.debug(System.out, debugIndent))
	        }
		if ((entry.dominanceFrontier intersect trailing.dominanceFrontier).nonEmpty) {
		  { debug("*struct* CONCAT trailing") }
		  ((init :+ stmt) ++ trailingStmts, nextTrailing)
		} else {
		  { debug("*struct* PASS trailing") }
		  (init :+ stmt, Some(trailing))
		}
	    }
	}
      case _ => (entry.body, None)
    }

    { debugIndent -= 2 }

    ret
  }

  def structTryCatch(entry: Block, exit: Block,
		     tcs: List[TryCatch]): Stmt = {
    val endIdcs = tcs map (_._2)
    val handlerIdcs = tcs map (_._3)
    val excns = tcs map (_._4)
    println(endIdcs.min)
    val endBlock = (blocks find (_.bound._1 == endIdcs.min)).get
    val stmts = entry match {
      case IfBlock(_, init, cond) if tcs.nonEmpty =>
	structIf(entry, exit, cond) match {
	  case (stmt, nextOpt) => init :+ stmt
	}
      case _ => entry.body
    }
    val handlerBlocks = handlerIdcs map (idx =>
      (blocks find (_.bound._1 == idx)).get)
    val catches = (handlerBlocks zip excns) map {
      case (block, excn) => Catch(struct(block, Some(exit))._1, excn)
    }
    Try(stmts ++ endBlock.body, catches: _*)
  }

  def structIf(entry: Block, exit: Block, cond: Cond): (Stmt, Option[Block]) = {
    { debugIndent += 2 }

    val nord = entry.ordinal
    val xord = exit.ordinal
    val nBlocks = xord - nord
    val nDomRet = entry.immediatelyDominated match {
      case Nil => 0
      case idomd => (idomd.init filter (_.successors.isEmpty)).length
    }
    val subseqBlocks = (((nord + 1) until xord) map blocks).toList
    val ifBlocks = (subseqBlocks takeWhile {
      case IfBlock(block, _, _) =>
	! (block.immediatelyDominated.nonEmpty &&
	   (block.immediatelyDominated.init exists (_.successors.isEmpty)))
      case _ => false
    } ) match {
      case Nil => Nil
      case blocks =>
	val idx = ((entry :: blocks) zip blocks) indexWhere {
	  case (blk0, blk1) =>
	    (blk0.successors intersect
	      (blk1.immediatelyDominated ++ blk1.dominanceFrontier)).isEmpty
	}
	if (idx < 0) blocks else blocks take idx
    }

    val lastIf = ifBlocks.lastOption getOrElse entry
    val thenEntry = blocks(nord + 1 + ifBlocks.length)
    val condx = combConds(entry, cond, ifBlocks, thenEntry)
    val thenExit = controlExit(lastIf).get

    { debug("*structIf* #"+ nord +"-#"+ xord)
      debug("*structIf* ifBlocks=")
      (entry :: ifBlocks) foreach (_.debug(System.out, debugIndent))
      debug("*structIf* thenEntry=")
      thenEntry.debug(System.out, debugIndent)
      debug("*structIf* thenExit=")
      thenExit.debug(System.out, debugIndent)
    }

    val thenStruct = struct(thenEntry, Some(thenExit))
    val trailing = thenStruct match {
      case (_, None) => lastIf.successors.lastOption
      case (_, Some(t)) => controlExit(t)
    }

    { debug("*structIf* trailing=")
      trailing foreach (_.debug(System.out, debugIndent))
    }

    val ifOnly = trailing match {
      case None => true
      case Some(_) if nDomRet > 0 => true
      case Some(t) if t == thenExit && ! (thenEntry.dominanceFrontier contains t) => false
      case Some(t) =>
	(thenEntry.dominanceFrontier intersect t.dominanceFrontier).isEmpty
    }

    val ret = if (ifOnly) {
      thenStruct match {
	case (stmts, None) =>
	  (If(condx, Then(stmts)), trailing)
	case (stmts, opt) =>
	  (If(condx, Then(stmts)), opt)
      }
    } else {
      val elseEntry = trailing.get
      val elseExit = controlExit(elseEntry).get

      { debug("*structIf* elseEntry=")
        elseEntry.debug(System.out, debugIndent)
        debug("*structIf* elseExit=")
        elseExit.debug(System.out, debugIndent)
      }

      val elseStruct = struct(elseEntry, Some(elseExit))
      val elseEntryJumps = (JumpBlock unapply elseEntry).isDefined
      val elseTrailing = if (elseExit == exit && elseEntryJumps) elseStruct._2
			 else Some(elseExit)

      { debug("*structIf* elseTrailing=")
        elseTrailing foreach (_.debug(System.out, debugIndent))
        debug("*structIf* __elseTrailing__=")
        elseStruct._2 foreach println
      }

      (If(condx,
	  Then(thenStruct._1),
	  Else(elseStruct._1)),
       elseTrailing)
    }

    { debugIndent -= 2 }

    ret
  }

  def combConds(entry: Block,
		cond: Cond,
		ifBlocks: List[Block],
		thenEntry: Block): Cond = ifBlocks match {
    case Nil => cond.invert
    case IfBlock(next, init, cond1) :: rest =>
      val succs = entry.successors
      val sect = succs intersect next.successors
      //LHA
      if (sect exists (_.ordinal < thenEntry.ordinal))
	combConds(next,
		  if (succs contains thenEntry) Or(cond, cond1)
		  else And(cond.invert, cond1), rest, thenEntry)
      //RHA
      else
	if (succs contains thenEntry)
	  Or(cond, combConds(next, cond1, rest, thenEntry))
	else
	  And(cond.invert, combConds(next, cond1, rest, thenEntry))
    case _ =>
      System.err.println("ERROR")
      System.err.println(" entry=")
      entry.debug(System.err, 2)
      System.err.println(" ifs=")
      ifBlocks foreach {
	_.debug(System.err, 2)
      }
      System.err.println(" thenEntry=")
      thenEntry.debug(System.err, 2)
      throw new RuntimeException("unrecognized if-structure entry="+ entry +
				 " ifs="+ (ifBlocks mkString ", ") +
				 " thenEntry="+ thenEntry)
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
      case (v, desc) => entry.local(v, Symbol("arg_"+ v), desc)
    }
    new MethodDecl(method.modifiers, method.name, method.desc,
		   argLocals, method.thrown, method.tryCatches, blocks)
  }
}
