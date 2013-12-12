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

  def structure(block: Block): List[Stmt] = {
    println(block)
    val init: List[Stmt] = block.body match {
      case _ :: Nil => Nil
      case body => body.init
    }
    val last: Stmt = block.body.last
    val stmtsAndNextBlock: (List[Stmt], Option[Block]) = last match {
      case If(cond, stmt) =>
	block.dominated match {
	  case sub :: Nil =>
	    println("1 "+ sub)
	    sub.dominanceFrontier match {
	      case block :: Nil =>
		(init :+ If(cond, Then(sub.body)),
		 Some(block))
	      case _ => throw new RuntimeException
	    }
	  case sub0 :: sub1 :: _ =>
	    println("2+ "+ sub0 +" "+ sub1)
	    sub0.dominanceFrontier match {
	      case block :: Nil =>
		(init :+ If(cond, Then(sub0.body), Else(structure(sub1))),
		 Some(block))
	      case _ => throw new RuntimeException
	    }
	}
      case _ =>
	println()
	(block.body, None)
    }
    val stmts = stmtsAndNextBlock._1
    val nextBlock = stmtsAndNextBlock._2
    stmts ++ ((nextBlock map structure) getOrElse Nil)
  }

  lazy val body: List[Stmt] = structure(blocks.head)

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
