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

import org.objectweb.asm.tree.analysis.Frame
import scala.collection.mutable.{BitSet, HashMap, MutableList, Stack}

import ast.Block

abstract class ControlFlowGraph(val method: MethodInfo) {
  def bounds: List[(Int, Int)]
  def edges: List[((Int, Int), (Int, Int))]
  def predecessors(beg: Int): List[(Int, Int)]
  def successors(end: Int): List[(Int, Int)]

  def predecessors(b: (Int, Int)): List[(Int, Int)] = predecessors(b._1)
  def successors(b: (Int, Int)): List[(Int, Int)] = successors(b._2)

  def mkblocks(frames: Array[Frame]): List[Block] = {
    var blocks: List[Block] = null//required for forward reference
    val getblocks: () => List[Block] = () => blocks
    blocks = bounds.zipWithIndex map {
      case (bound, ord) =>
	new Block(ord, bound, method, frames, this, getblocks)
    }
    blocks
  }

  def singleExitReverse: ControlFlowGraph = new ControlFlowGraph(method) {
    val cfg = ControlFlowGraph.this
    val dummyExit: (Int, Int) = (-1, -1)
    val bounds = dummyExit :: cfg.bounds.reverse
    val exits: List[(Int, Int)] =
      (cfg.bounds filter (b => (cfg successors b).isEmpty)).reverse
    val edges = (bounds map (b => successors(b) map (b -> _))).flatten

    def predecessors(beg: Int) = (cfg predecessors beg).reverse
    def successors(end: Int) = (cfg successors end).reverse

    override def predecessors(b: (Int, Int)): List[(Int, Int)] =
      if (b == dummyExit) Nil
      else if (successors(b._2).isEmpty) List(dummyExit)
      else successors(b._2)
    override def successors(b: (Int, Int)): List[(Int, Int)] =
      if (b == dummyExit) exits else predecessors(b._1)
  }

  case class Node(n: Int, b: (Int, Int), succs: List[(Int, Int)]) {
    def isAncestorOf(t: Tree, m: Int): Boolean = (t parent m) match {
      case None => false
      case Some(parent) => if (parent.n == n) true else isAncestorOf(t, parent.n)
    }

    def isDescendantOf(t: Tree, m: Int): Boolean = (t parent n) match {
      case None => false
      case Some(parent) =>
	if (parent.n == m) true else parent.isDescendantOf(t, m)
    }

    override def toString = "node n="+ n +" b="+ b
  }

  /* Vigdorchik
   * https://scfa.googlecode.com/svn/trunk/src/cfa/dfst.scala
   */
  abstract class Tree {
    def size: Int
    def pre(n: Int): Node
    def post(n: Int): Node
    def parent(n: Int): Option[Node]
    def edgeKinds: HashMap[(Int, Int), EdgeKind]

    def preorder: IndexedSeq[Node] = (0 until size) map (pre)
    def postorder: IndexedSeq[Node] = (0 until size) map (post)
    def parents: IndexedSeq[Option[Node]] = (0 until size) map (parent)

    def children(n: Int): IndexedSeq[Node] = preorder filter { node =>
      parent(node.n) match {
	case None => false; case Some(p) => p.n == n
      }
    }
  }

  lazy val dfst: Tree = {
    println(bounds)
    val pre0 = Array.fill(bounds.length)(-1)
    val post0 = Array.fill(bounds.length)(-1)
    val pre1 = new Array[Node](bounds.length)
    val post1 = new Array[Node](bounds.length)
    val ps = new Array[Node](bounds.length)
    val eks: HashMap[(Int, Int), EdgeKind] = HashMap.empty
    def catchBounds(beg: Int): List[(Int, Int)] =
      method.tryCatches filter(_._1 == beg) map (tc =>
	bounds.find(b => b._1 == tc._3).get)
    def f(nodes: List[Node], m: Int, n: Int): Int = nodes match {
      case Nil => m
      case node :: nt => node.succs match {
	case Nil =>
	  post0(node.n) = n
	  post1(n) = node
          f(nt, m, n - 1)
	case succ :: st =>
	  val sidx = bounds indexOf succ
          val sn = Node(sidx, succ, successors(succ) ++ catchBounds(succ._1))
	  val ssn = Node(node.n, node.b, st)
          if (pre0(sn.n) >= 0 && post0(sn.n) < 0) {
	    eks(node.n -> sidx) = Back
            f(ssn :: nt, m, n)
          } else if (pre0(sn.n) >= 0 && post0(sn.n) >= 0) {
	    eks(node.n -> sidx) =
	      if (pre0(node.n) > pre0(sn.n)) Cross else Forward
            f(ssn :: nt, m, n)
          } else {
	    pre0(sn.n) = m
	    pre1(m) = sn
	    ps(sn.n) = node
	    eks(node.n -> sidx) = Tree
            f(sn :: ssn :: nt, m + 1, n)
          }
      }
    }
    val entry = Node(0, bounds.head, successors(bounds.head) ++ catchBounds(0))
    pre0(0) = 0
    pre1(0) = entry
    new Tree {
      val size = f(entry :: Nil, 1, bounds.length - 1)
      assert(size == bounds.length)
      def pre(n: Int) = pre1(n)
      def post(n: Int) = post1(n)
      def parent(n: Int) = ps(n) match {
	case null => None
	case parent => Some(parent)
      }
      val edgeKinds = eks
    }
  }

  /* Cooper, Harvey, Kennedy; Rice
   * A Simple, Fast Dominance Algorithm
   * www.hipersoft.rice.edu/grads/publications/dom14.pdf
   */
  lazy val immediateDominators: List[Option[(Int, Int)]] = {
    val tree = dfst
    val bs = bounds
    val rpo = tree.postorder.reverse
    //val rpons = rpo map (_.n)
    val rpobs = rpo map (_.b)
    val idoms = new Array[(Int, Int)](tree.size)
    idoms(0) = bs(0)
    var changed = true
    while (changed) {
      changed = false
      for (n <- rpo.init) {
	var idom: (Int, Int) = null
	for (pred <- predecessors(n.b) if idoms(bs indexOf pred) != null) {
	  if (idom == null)
	    idom = pred
	  else {
	    var b1 = idom
	    var b2 = pred
	    while (b1 != b2) {
	      while ((rpobs indexOf b1) < (rpobs indexOf b2))
		b1 = idoms(bs indexOf b1)
	      while ((rpobs indexOf b2) < (rpobs indexOf b1))
		b2 = idoms(bs indexOf b2)
	    }
	    idom = b1
	  }
	}
	if (idoms(n.n) != idom) {
	  idoms(n.n) = idom
	  changed = true
	}
      }
    }
    (idoms map (idom =>
      if (idom == null) None else Some(idom))).toList
  }

  lazy val immediatelyDominatedBy: Array[BitSet] = {
    val idomd = Array.fill(bounds.length)(BitSet.empty)
    for (i <- 0 until bounds.length) {
      immediateDominators(i) match {
	case Some(idom) =>
	  idomd(bounds indexOf idom) += i
	case None =>
      }
    }
    idomd
  }

  lazy val dominanceFrontiers: Array[BitSet] = {
    val dfs = Array.fill(bounds.length)(BitSet.empty)
    for (idx <- 0 until bounds.length) {
      val preds = predecessors(bounds(idx))
      if (preds.length > 1)
	for (pred <- preds) {
	  var run = pred
	  val idom = immediateDominators(idx)
	  if (idom.isDefined) {
	    while (run != idom.get) {
	      val runIdx = bounds indexOf run
	      dfs(runIdx) += idx
	      run = immediateDominators(runIdx) getOrElse idom.get
	    }
	  }
	}
    }
    dfs
  }
}
