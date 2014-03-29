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

import asm._
import org.objectweb.asm.tree.analysis.Frame

object AnchorFloatingStmts extends MethodInfo.CFGTransform {
  val interpreter = MethodInfo.basicInterpreter

  def apply(method: MethodInfo,
	    frames: Array[Frame],
	    cfg: ControlFlowGraph): MethodInfo.Changes = {
    var curLocal = method.node.maxLocals
    def nextLocal(desc: Option[String]): Int = desc match {
      case Some(wide) if (wide equals "J") || (wide equals "D") =>
	curLocal += 2; curLocal - 2
      case _ =>
	curLocal += 1; curLocal - 1
    }
    def isExpr(insn: Insn): Boolean = insn match {
      case label() => true
      case _ => insnPushes(insn)
    }
    val insns = method.instructions
    val zBounds = stackZeroBounds(frames)
    println("zBounds="+ zBounds)
    val hits = for ((zBeg, zEnd) <- zBounds) yield {
      val stmts =
	for ((insn, idx) <- insns.slice(zBeg, zEnd - 1).zipWithIndex
	     if ! isExpr(insn)) yield {
	       var stmtIdx = zBeg + idx
	       val nextFrame = frames(stmtIdx + 1)
	       //println("*************************************************")
	       //insns.slice(zBeg, stmtIdx + 1) map insnString foreach println
	       val stmtBeg = ((zBeg until stmtIdx).reverse find (x =>
		 frames(x).getStackSize == nextFrame.getStackSize)) match {
		   case Some(idx) => idx
		   case None =>
		     throw new RuntimeException("no eq stack frame w/i "+
						zBeg +"..."+stmtIdx)
		 }
	       insns(stmtIdx) match {
		 case JumpInsnNode(_, lbl) =>
		   val ord = cfg.bounds.indexWhere(_._2 == stmtIdx + 1)
		   //println("df"+ cfg.bounds(ord) +"="+ (cfg.dominanceFrontiers(ord) map cfg.bounds))
		   stmtIdx = cfg.bounds(cfg.dominanceFrontiers(ord).head)._1
		 case _ =>
	       }
	       (stmtBeg, stmtIdx)
	     }
      (zBeg, stmts.toList, zEnd)
    }
    hits filter (_._2.nonEmpty) foreach println
    def func(x: List[List[(Int, Int, Option[String])]]): List[(Int, Int, Option[String])] =
      x.reduceLeft((left, right) =>
	((left.init zip right) map {
	  case ((lsIdx, llIdx, lDesc),
		(rsIdx, rlIdx, rDesc)) =>
		  println("***l "+lsIdx+","+llIdx+","+lDesc)
		  println("***r "+rsIdx+","+rlIdx+","+rDesc)
		  (lsIdx, rlIdx, rDesc)
	} ) :+ left.last)
    val spec: MethodInfo.InsnSpec = (hits map {
      case (_, Nil, _) => Nil
      case (zBeg, stmts, zEnd) =>
	(if (insns.haveSideEffects(zBeg, stmts.last._1)) {
	  val preSubSpecs = stmts map {
	    case (stmtBeg, stmtIdx) => stack(frames(stmtBeg)).zipWithIndex map {
	      case ((_, value), stackIdx) =>
		val storeIdx = (zBeg to stmtBeg).reverse.find(idx =>
		  frames(idx).getStackSize == stackIdx + 1).get
		val loadIdx = stmtIdx + 1
		val desc = valueDesc(value)
		(storeIdx, loadIdx, valueDesc(value))
	    }
	  }
	  println("***\n"+ preSubSpecs.mkString("\n"))
	  val xyz: List[(Int, Int, Option[String])] = preSubSpecs match {
	    case _ :: Nil => preSubSpecs.flatten
	    case _ :: _ :: Nil => func(preSubSpecs)
	    case _ => preSubSpecs.head ++ func(preSubSpecs.tail)//i think this is sufficient
	    //is it?
	  }
	  val subSpec = xyz map {
	    case (storeIdx, loadIdx, desc) =>
	      var loc = nextLocal(desc)
	      (storeIdx, storeIdx) -> List(store(loc, desc)) ::
	      (loadIdx, loadIdx)   -> List(load(loc, desc))  :: Nil
	  }
	  subSpec
	} else {
	  val subSpec = stmts map {
	    case (stmtBeg, stmtIdx) =>
	      val stmtFrame = frames(stmtIdx)
	      val stmtEnds = ((stmtBeg to stmtIdx) filter (idx =>
		frames(idx).getStackSize == stmtFrame.getStackSize)).toList
	      //println("zBeg="+ zBeg)
	      //println("stmtBeg="+ stmtBeg)
	      //println("###"+ stmtEnds)
	      val stmtEnd = stmtEnds match {
		case end :: Nil => end
		case end0 :: end1 :: Nil =>
		  if ((end0 until end1) map (end =>
		    insnPushes(insns(end))) reduce (_ & _)) end1 else end0
		case _ => throw new RuntimeException
	      }
	      //println("stmtEnd="+ stmtEnd)
	      //println("stmtIdx="+ stmtIdx)
	      val stmt = (insns.slice(stmtBeg, stmtEnd).toList map insnClone) :+
			  insnClone(insns(stmtIdx))
	      (stmtIdx, stmtIdx + 1) -> Nil  ::
	      (stmtBeg, stmtEnd)     -> Nil  ::
	      (zBeg, zBeg)           -> stmt :: Nil
	  }
	  subSpec
	} ).flatten
    } ).flatten
    MethodInfo.Changes(None, Some(curLocal), spec)
  }
}
