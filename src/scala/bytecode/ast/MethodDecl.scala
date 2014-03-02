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

  def combIfs(head: Block, cond: Cond, ifs: List[Block], theN: Block): Cond =
    ifs match {
      case Nil => cond.invert
      case IfBlock(next, _, cond1) :: rest =>
	val succs = head.successors
	val sect = succs intersect next.successors
	if (sect exists (_.ordinal < theN.ordinal))//lha
	  combIfs(next, if (succs contains theN) Or(cond, cond1)
			else And(cond.invert, cond1), rest, theN)
	else//rha
	  if (succs contains theN)
	    Or(cond, combIfs(next, cond1, rest, theN))
	  else
	    And(cond.invert, combIfs(next, cond1, rest, theN))
    }

  def structIf(head: Block, cond: Cond): Stmt = {
    val hord = head.ordinal
    val exit = head.dominanceFrontier match {
      case Nil => head.dominated.last
      case x :: Nil => x
      case x =>
	println(x mkString "; ")
	throw new RuntimeException
    }
    println("head="+head)
    println("exit="+exit)
    println("head_ds="+head.dominated.mkString("; "))
    println("head_df="+head.dominanceFrontier.mkString("; "))
    val n = exit.ordinal - head.ordinal
    if ((head span exit).size % 2 == 0) {//TODO deal with else structure (elif)
      val ifBlocks = (1 until n - 2).toList map (m => blocks(hord + m))
      val thenBlock = exit.predecessors.init.last
      val elseBlock = exit.predecessors.last
      If(combIfs(head, cond, ifBlocks, thenBlock),
	 Then(thenBlock.body), Else(elseBlock.body))
    } else {
      val ifBlocks = (1 until n - 1).toList map (m => blocks(hord + m))
      val thenBlock = exit.predecessors.last
      If(combIfs(head, cond, ifBlocks, thenBlock),
	 Then(thenBlock.body))
    }
  }

  def structure(head: Block): List[Stmt] = (head match {
    case IfBlock(_, init, cond) =>
      val stmts: List[Stmt] = init :+ structIf(head, cond)
      head.dominanceFrontier match {
	case Nil => (stmts, Some(head.dominated.last))
	case next :: Nil => (stmts, Some(next))
	case h :: x =>
	  println("~~~"+h+" :: "+x)
	  throw new RuntimeException
	case _ => (stmts, None)
      }
    case _ => (head.body, None)
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
    /*blocks foreach { block =>
      println(block)
      println("dominated by: "+ block.dominator)
      println("dominates: "+ block.dominated)
      println("dominance frontier: "+ block.dominanceFrontier)
    }*/
    new MethodDecl(method.modifiers,
		   method.name,
		   method.desc,
		   argLocals,
		   method.thrown,
		   blocks)
  }
}
