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

import scala.bytecode.MethodInfo

class MethodDecl(val modifiers: List[Symbol],
		 val name: String,
		 val desc: String,
		 val argLocals: List[Local],
		 val thrown: List[String],
		 val blocks: List[Block]) extends Exec {
  val returnDesc: String = desc substring ((desc indexOf ')') + 1)
  val body: List[Stmt] = structure(blocks.head)

  def combIfs(entry: Block, cond: Cond,
	      ifBlocks: List[Block], thenBlock: Block): Cond = ifBlocks match {
    case Nil => cond.invert
    case IfBlock(next, init, cond1) :: rest =>
      val succs = entry.successors
      val sect = succs intersect next.successors
      val discont = next.dominated match {
	case IfBlock(sub, _, _) :: _ => next.ordinal + 1 != sub.ordinal
	case _ => false
      }
      //LHA
      if (sect exists (_.ordinal < thenBlock.ordinal))
	combIfs(next,
		if (succs contains thenBlock) Or(cond, cond1)
		else And(cond.invert, if (discont) cond1.invert
				      else cond1), rest, thenBlock)
      //RHA
      else
	if (succs contains thenBlock)
	  Or(cond, combIfs(next, cond1, rest, thenBlock))
	else
	  And(cond.invert, combIfs(next, cond1, rest, thenBlock))
  }

  def structIf(entry: Block, exit: Block, cond: Cond): Stmt = {
    val ord = entry.ordinal
    val n = exit.ordinal - ord
    println("n_ifs="+ n +" (entry span exit)="+ (entry span exit).size)
    if ((entry span exit).size % 2 == 0) {//TODO deal with else structure (elif)
      val ifBlocks = (1 until n - 2).toList map (m => blocks(ord + m))
      val thenBlock = exit.predecessors.init.last
      val elseBlock = exit.predecessors.last
      If(combIfs(entry, cond, ifBlocks, thenBlock),
	 Then(thenBlock.body), Else(elseBlock.body))
    } else {
      val ifBlocks = (1 until n - 1).toList map (m => blocks(ord + m))
      val thenBlock = exit.predecessors.last
      If(combIfs(entry, cond, ifBlocks, thenBlock),
	 Then(thenBlock.body))
    }
  }

  def ifExit(entry: Block): Block = entry.dominanceFrontier match {
    case Nil => entry.dominated.last
    case IfBlock(exit, _, _) :: _ =>
      if ((entry.ordinal until exit.ordinal) map (i => blocks(i) match {
	case IfBlock(_, _, _) => true; case _ => false
      } ) reduce (_ & _)) entry.dominated.last else exit
    case xs =>
      val x = xs.last
      val y = entry.dominated.last
      if (x.ordinal > y.ordinal) x else y
  }

  def structure(entry: Block): List[Stmt] = (entry match {
    case IfBlock(_, init, cond) =>
      val exit = ifExit(entry)
      println("entry="+ entry +" exit="+ exit)
      val stmts: List[Stmt] = init :+ structIf(entry, exit, cond)
      (stmts, Some(exit))
    case _ => (entry.body, None)
  } ) match {
    case (stmts, None) => stmts
    case (stmts, Some(next)) => stmts ++ structure(next)
  }

  def out(ps: java.io.PrintStream, indent: Int) {
    ps append " "* indent
    if (modifiers.nonEmpty) {
      ps append (modifiers map (_.name) mkString " ")
      ps append " "
    }
    ps append returnDesc
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

import org.objectweb.asm.tree.analysis.Analyzer
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
		   argLocals, method.thrown, blocks)
  }
}
