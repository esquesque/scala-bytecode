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
    val floatingStmtsIdcs =
      for ((beg, end) <- zBounds)
	yield for ((insn, idx) <- insns.slice(beg, end - 1).zipWithIndex
		   if ! insnPushes(insn)) yield beg + idx
    val spec: MethodInfo.InsnSpec = ((zBounds zip floatingStmtsIdcs) map {
      case (zBound, stmtIdcs) => (stmtIdcs map { stmtIdx =>
	val stmtNextFrame = frames(stmtIdx + 1)
        val zBeg = zBound._1
	val opndBeg = ((zBeg until stmtIdx).reverse find (idx =>
	  frames(idx).getStackSize == stmtNextFrame.getStackSize)).get
	/* If intervening instructions have side effects, split them into zeroed
	 * stores and a sequence of equivalent loads before the statement in
	 * order to emulate the stack without causing side effects out of order.
	 */
        if (insns.haveSideEffects(zBeg, opndBeg)) {
	  val preSpec: List[(Int, Int, Option[String])] = {
	    stack(frames(opndBeg)) map {
	      case (depth, value) =>
	        val insertIdx = (zBeg to opndBeg).find(idx =>
		  frames(idx).getStackSize == depth).get
	      val desc = valueDesc(value)
	      (insertIdx, nextLocal(desc), desc)
	    }
	  }
	  (preSpec map { case (insertIdx, loc, desc) =>
	    (insertIdx, insertIdx) -> List(store(loc, desc))
	  } ) ++
	  (preSpec map { case (_, loc, desc) =>
	    (stmtIdx + 1, stmtIdx + 1) -> List(load(loc, desc))
	  } )
	}
	//Otherwise, just move the statement to the stack zero.
	else {
	  val stmtFrame = frames(stmtIdx)
	  val opndEnds = ((opndBeg to stmtIdx) filter (idx =>
	    frames(idx).getStackSize == stmtFrame.getStackSize)).toList
	  val opndEnd = opndEnds match {
	    case idx :: Nil => idx
	    case idx0 :: idx1 :: Nil =>
	      if ((idx0 until idx1) map (idx =>
		insnPushes(insns(idx))) reduce (_ & _)) idx1 else idx0
	    case _ => throw new RuntimeException
	    //I don't think this is possible but it could be
	    //TODO rework
	  }
	  val stmt = (insns.slice(opndBeg, opndEnd).toList map insnClone) ++
		       (insnClone(insns(stmtIdx)) :: Nil)
	  (zBeg, zBeg) -> stmt ::
	  (opndBeg, opndEnd) -> Nil ::
	  (stmtIdx, stmtIdx + 1) -> Nil :: Nil
	}
      } ).flatten
    } ).flatten
    MethodInfo.Changes(None, Some(curLocal), spec)
  }
}
