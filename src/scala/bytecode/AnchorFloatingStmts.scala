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

object AnchorFloatingStmts extends MethodInfo.AnalyzeBasicTransform {
  def apply(info: MethodInfo, frames: Array[Frame]): MethodInfo.Changes = {
    //info.instructions.out()
    var curLocal = info.node.maxLocals
    def nextLocal(desc: Option[String]): Int = desc match {
      case Some(wide) if (wide equals "J") || (wide equals "D") =>
	curLocal += 2; curLocal - 2
      case _ =>
	curLocal += 1; curLocal - 1
    }
    val zBounds = stackZeroBounds(frames)
    val insns = info.instructions
    val hits = for ((zBeg, zEnd) <- zBounds) yield {
      val stmts = for ((insn, idx) <- insns.slice(zBeg, zEnd - 1).zipWithIndex
		       if ! insnPushes(insn))
		  yield {
		    val stmtIdx = zBeg + idx
		    val nextFrame = frames(stmtIdx + 1)
		    val stmtBeg = ((zBeg until stmtIdx).reverse find (idx =>
		      frames(idx).getStackSize == nextFrame.getStackSize)).get
		    (stmtBeg, stmtIdx)
		  }
      (zBeg, stmts.toList, zEnd)
    }
    val spec: MethodInfo.InsnSpec = (hits map {
      case (_, Nil, _) => Nil
      case (zBeg, stmts, zEnd) =>
	(if (stmts exists {
	  case (stmtBeg, _) => insns.haveSideEffects(zBeg, stmtBeg)
	} ) {
	  val preSpecs = stmts map {
	    case (stmtBeg, stmtIdx) => stack(frames(stmtBeg)) map {
	      case (depth, value) =>
		val storeIdx = (zBeg to stmtBeg).find(idx =>
		  frames(idx).getStackSize == depth).get
		val loadIdx = stmtIdx + 1
		val desc = valueDesc(value)
		(storeIdx, loadIdx, valueDesc(value))
	    }
	  }
	  preSpecs.reduceLeft((left, right) =>
	    ((left.init zip right) map {
	      case ((lStoreIdx, lLoadIdx, lDesc),
		    (rStoreIdx, rLoadIdx, rDesc)) =>
		(lStoreIdx, rLoadIdx, rDesc)
	    } ) :+ left.last) map {
	      case (storeIdx, loadIdx, desc) =>
		var loc = nextLocal(desc)
		(storeIdx, storeIdx) -> List(store(loc, desc)) ::
		(loadIdx, loadIdx) -> List(load(loc, desc)) :: Nil
	    }
	} else {
	  stmts map {
	    case (stmtBeg, stmtIdx) =>
	      val stmtFrame = frames(stmtIdx)
	      val stmtEnds = ((stmtBeg to stmtIdx) filter (idx =>
		frames(idx).getStackSize == stmtFrame.getStackSize)).toList
	      val stmtEnd = stmtEnds match {
		case idx :: Nil => idx
		case idx0 :: idx1 :: Nil =>
		  if ((idx0 until idx1) map (idx =>
		    insnPushes(insns(idx))) reduce (_ & _)) idx1 else idx0
		case _ => throw new RuntimeException
	      }
	      val stmt = (insns.slice(stmtBeg, stmtEnd).toList map insnClone) :+
			  insnClone(insns(stmtIdx))
	      (zBeg, zBeg) -> stmt ::
	      (stmtBeg, stmtEnd) -> Nil ::
	      (stmtIdx, stmtIdx + 1) -> Nil :: Nil
	  }
	} ).flatten
    } ).flatten
    MethodInfo.Changes(None, Some(curLocal), spec)
  }
}
