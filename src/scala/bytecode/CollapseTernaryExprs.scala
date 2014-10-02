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

object CollapseTernaryExprs extends MethodInfo.CFGTransform {
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
    val tcs = method.tryCatches
    val nonZeroBlockBounds = cfg.bounds filter {
      case (beg, end) if ! (tcs exists (_._3 == beg)) =>
	val begDepth = frames(beg).getStackSize
	val endDepth = if (end == frames.length) 0 else frames(end).getStackSize
	//println((beg, end))
	//println("depth "+ (begDepth, endDepth))
	begDepth > 0 && begDepth > endDepth
      case _ => false
    }
    val spec: MethodInfo.InsnSpec =
      (for (bound <- nonZeroBlockBounds) yield {
	val ord = cfg.bounds indexOf bound
	//println("!!!"+ bound)
	val head: Option[(Int, Int)] =
	  cfg.dominanceFrontiers(ord).toList match {
	    case Nil => cfg.immediateDominators(ord)
	    case df => cfg.immediateDominators(df.head)
	  }
	if (head.isDefined) {
	  val domEndDepth =
	    frames(head.get._2).getStackSize
	  //println("dom end depth "+ domEndDepth)
	  val begDepth = frames(bound._1).getStackSize
	  val localSpecs = stackDescs(frames(bound._1), domEndDepth) map (desc =>
	    (nextLocal(desc), desc))
	  val loads = localSpecs map { case (v, desc) => load(v, desc) }
	  (for (pred <- cfg predecessors bound) yield {
            val insertIdx = ((pred._1 until pred._2).reverse find (idx =>
	      frames(idx + 1).getStackSize == begDepth)).get + 1
            val stores = localSpecs map { case (v, desc) => store(v, desc) }
            (insertIdx, insertIdx) -> stores
	  } ) ++ ((bound._1 + 1, bound._1 + 1) -> loads :: Nil)
	} else Nil
      } ).flatten
    MethodInfo.Changes(None, Some(curLocal), spec)
  }
}
