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
  val returnType: Type = Type.getReturnType(desc)
  val body: List[Stmt] = structureBody(blocks.head)

  def combConds(entry: Block, cond: Cond,
	      ifBlocks: List[Block], thenEntry: Block): Cond = ifBlocks match {
    case Nil => cond.invert
    case IfBlock(next, init, cond1) :: rest =>
      val succs = entry.successors
      val sect = succs intersect next.successors
      val discont = next.dominated match {
	case IfBlock(sub, _, _) :: _ => next.ordinal + 1 != sub.ordinal
	case _ => false
      }
      //LHA
      if (sect exists (_.ordinal < thenEntry.ordinal))
	combConds(next,
		  if (succs contains thenEntry) Or(cond, cond1)
		  else And(cond.invert, if (discont) cond1.invert
					else cond1), rest, thenEntry)
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

  def structIf(entry: Block, exit: Block, cond: Cond): (Stmt, Option[Block]) = {
    val nord = entry.ordinal
    val xord = exit.ordinal
    val nBlocks = xord - nord
    val nDomRet = (entry.dominated.init filter (_.successors.isEmpty)).length
    val subseqBlocks = (((nord + 1) until xord) map blocks).toList
    val ifBlocks = (subseqBlocks takeWhile {
      case IfBlock(block, _, _) =>
	! (block.dominates &&
	   (block.dominated.init exists (_.successors.isEmpty)))
      case _ => false
    } ) match {
      case Nil => Nil
      case blocks =>
	val idx = ((entry :: blocks) zip blocks) indexWhere {
	  case (blk0, blk1) =>
	    (blk1.successors intersect blocks).isEmpty &&
	    (blk0.successors intersect blk1.successors).isEmpty
	}
	if (idx < 0) blocks else blocks take idx
    }
    val thenEntry = blocks(nord + 1 + ifBlocks.length)
    val thenExit = scopeExit(thenEntry) getOrElse blocks(thenEntry.ordinal + 1)

    println("structIf2 #"+ nord +"-#"+ xord +
	    "\n nBlocks="+ nBlocks +
	    "\n nDomRet="+ nDomRet +
	    "\n ifBlocks="+ ifBlocks +
	    "\n thenEntry="+ thenEntry +
	    "\n thenExit="+ thenExit)

//               fix this!
    val noElse = if (thenExit != exit) thenExit.ordinal - thenEntry.ordinal <= 1
		 else xord - thenEntry.ordinal <= 1
    //val noElse = m % 2 != 0
    var elseExit: Block = null
    val stmt =
      if (noElse) {
	If(combConds(entry, cond, ifBlocks, thenEntry),
	   Then(struct(thenEntry, Some(thenExit))._1))
      } else {
	println("***"+ ifBlocks.lastOption.getOrElse(entry).dominated)
	val elseEntry =
	  //if (ifBlocks.isEmpty && thenExit.ordinal < exit.ordinal) thenExit
	  /*else */ifBlocks.lastOption.getOrElse(entry).dominated match {
	    case domd if domd.last.successors.isEmpty ||
			 domd.last == thenExit => domd.init.last
	    case domd => domd.last
	  }
	//val elseEntry = blocks(thenExit.ordinal - 1)
	println(" *elseEntry="+ elseEntry)
	elseExit = scopeExit(elseEntry).get
	println(" *elseExit="+ elseExit)
	If(combConds(entry, cond, ifBlocks, thenEntry),
	   Then(struct(thenEntry, Some(thenExit))._1),
	   Else(struct(elseEntry, Some(elseExit))._1))
    }
    println(" stmt="+ stmt)
    //it's handling trailing wrong... hhmmmm
    //should never check by ordinal
    //should trailing be used as an exit block correction?
    val trailing =
      if (noElse) {
	if (thenExit.ordinal < exit.ordinal) Some(thenExit) else None
      } else {
	if (elseExit.ordinal < exit.ordinal) Some(elseExit) else None
      }
    if (trailing.isDefined) println(" *trailing="+ trailing.get)
    (stmt, trailing)//for now
  }

/*  def structIf(entry: Block, exit: Block, cond: Cond): Stmt = {
    structIfTrailing(entry, exit, cond) match {
      case (stmt, trailing) =>
	println("..."+ stmt)
	println("...trailingEntry="+ trailing)
    }
    val ord = entry.ordinal

    val n = exit.ordinal - ord
    val nReturned = (entry.dominated.init filter (_.successors.isEmpty)).length
    //this have been observed to be either 0 for a non-returning structure
    //or 1 for a returning structure (if-then-return-else/exit)
    val m = (entry span exit).size + nReturned
    //I don't know why, but m is an even number for else-structures. This is its
    //only use.

    println("structIf #"+ entry.ordinal +" n="+ n +" m="+ m +" nReturned="+ nReturned)

    if (m % 2 == 0) {//TODO deal with else structure (elif)
      if (nReturned == 0) {
	val ifBlocks = (1 until n - 2).toList map (x => blocks(ord + x))
	val thenBlock = exit.predecessors.init.last
	val elseBlock = exit.predecessors.last
	assert(elseBlock.ordinal + 1 == exit.ordinal)
	If(combConds(entry, cond, ifBlocks, thenBlock),
	   Then(struct(thenBlock, Some(elseBlock))),
	   Else(struct(elseBlock, Some(exit))))
      } else {
	val ifBlocks = (1 until n - 3).toList map (x => blocks(ord + x))
	val thenBlock = blocks(ord + 2)
	val elseBlock = exit.predecessors.head
	//assert(elseBlock.ordinal + 1 == exit.ordinal)
	If(combConds(entry, cond, ifBlocks, thenBlock),
	   Then(struct(thenBlock, Some(elseBlock))),
	   Else(struct(elseBlock, Some(exit))))
      }
    } else {
      if (nReturned == 0) {
	val ifBlocks = (1 until n - 1).toList map (m => blocks(ord + m))
	val thenBlock = exit.predecessors.last
	assert(thenBlock.ordinal + 1 == exit.ordinal)
	//hack
	(ifBlocks indexWhere (_.successors.isEmpty)) match {
	  case -1 =>
	    If(combConds(entry, cond, ifBlocks, thenBlock),
	     Then(struct(thenBlock, scopeExit(thenBlock))))
	  case idx =>
	    val ifBlocks1 = ifBlocks.slice(0, idx)
	    val thenBlock1 = ifBlocks(idx)
	    If(combConds(entry, cond, ifBlocks1, thenBlock1),
	       Then(struct(thenBlock1, scopeExit(thenBlock1))))
	}
	    
      } else {
	val ifBlocks = (1 until n - 2).toList map (m => blocks(ord + m))
	val thenBlock = blocks(ord + n - 1)
	assert(thenBlock.ordinal + 1 == exit.ordinal)
	If(combConds(entry, cond, ifBlocks, thenBlock),
	   Then(struct(thenBlock, scopeExit(thenBlock))))
      }
    }
  }*/

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

  /* i still haven't grokked this pattern */
  def scopeExit(entry: Block): Option[Block] = entry.dominanceFrontier match {
    case Nil if entry.dominates => Some(entry.dominated.last)
    case Nil => None
    case IfBlock(exit, _, _) :: Nil
	 if entry.dominated exists (_.successors.isEmpty) =>
      Some(entry.dominated.last)
    case IfBlock(exit, _, _) :: Nil =>
      Some(if ((entry.ordinal until exit.ordinal) map (i => blocks(i) match {
	case IfBlock(_, _, _) => true; case _ => false
      } ) reduce (_ & _)) entry.dominated.last else exit)
    case df if entry.dominated exists (_.successors.isEmpty) =>
      val ldf = df.last
      val ldom = entry.dominated.last
      Some(if (ldf.ordinal < ldom.ordinal) ldf else ldom)
    case xs if entry.dominates =>
      val x = xs.last
      val y = entry.dominated.last
      Some(if (x.ordinal < y.ordinal &&
	       ! (x.dominanceFrontier contains y)) x else y)
    case xs =>
      Some(xs.last)
  }

  def struct(entry: Block, exit: Option[Block]): (List[Stmt], Option[Block]) = {
    println("struct\n entry=")
    entry.debug(System.out, 2)
    println(" exit=")
    exit foreach (_.debug(System.out, 2))
    val enteringTryCatches = tryCatches filter (_._1 == entry.bound._1)
    entry match {
      case _ if enteringTryCatches.nonEmpty =>
        (structTryCatch(entry, exit.get, enteringTryCatches) :: Nil, None)
      case IfBlock(_, init, cond) =>
	structIf(entry, exit.get, cond) match {
	  case (stmt, nextOpt) => (init :+ stmt, nextOpt)
	}
      case _ => (entry.body, None)
    }
  }

  def structureBody(entry: Block): List[Stmt] = {
    val exit = scopeExit(entry)
    struct(entry, exit) match {
      case (stmts, Some(next)) =>
	println("structureBody\nnext=")
	next.debug(System.out, 2)
	println("exit=")
	exit.get.debug(System.out, 2)
	stmts ++ structureBody(next)
      case (stmts, None) if exit.isDefined => stmts ++ structureBody(exit.get)
      case (stmts, None) => stmts
    }
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
    val blocks = cfg.mkblocks(frames)
    val entry = blocks.head
    val argLocals = method.arguments map {
      case (v, desc) => entry.local(v, Symbol("arg_"+ v), desc)
    }
    new MethodDecl(method.modifiers, method.name, method.desc,
		   argLocals, method.thrown, method.tryCatches, blocks)
  }
}
