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
import scala.collection.mutable.BitSet

import ast.Block

abstract class ControlFlowGraph(val method: MethodInfo) {
  def bounds: List[(Int, Int)]
  def edges: List[((Int, Int), (Int, Int))]
  def predecessors(beg: Int): List[(Int, Int)]
  def successors(end: Int): List[(Int, Int)]

  def predecessors(b: (Int, Int)): List[(Int, Int)] = predecessors(b._1)
  def successors(b: (Int, Int)): List[(Int, Int)] = successors(b._2)

  def mkblocks(frames: Array[Frame]): List[Block] = {
    var blocks: List[Block] = null
    val getblocks: () => List[Block] = () => blocks
    blocks = bounds.zipWithIndex map {
      case (bound, ord) =>
	new ast.Block(ord, bound, method, frames, this, getblocks)
    }
    blocks
  }

  case class Node(n: Int, b: (Int, Int), succs: List[(Int, Int)])

  /* traversal tree
   */
  abstract class Tree {
    def size: Int
    def pre(n: Int): Node
    def post(n: Int): Node

    def preorder: IndexedSeq[Node] = (0 until size) map (pre)
    def postorder: IndexedSeq[Node] = (0 until size) map (post)
  }

  /* depth first search tree...
   * fuck you... pretty slow... todo optimize
   */
  lazy val dfst: Tree = {
    val pre0 = Array.fill(bounds.length)(-1)
    val post0 = Array.fill(bounds.length)(-1)
    val pre1 = new Array[Node](bounds.length)
    val post1 = new Array[Node](bounds.length)
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
          if (pre0(sn.n) >= 0 && post0(sn.n) < 0)
            f(ssn :: nt, m, n)
          else if (pre0(sn.n) >= 0 && post0(sn.n) >= 0)
            f(ssn :: nt, m, n)
          else {
	    pre0(sn.n) = m
	    pre1(m) = sn
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
    }
  }

  /* adapted from "A Simple, Fast Dominance Algorithm"
   *   www.hipersoft.rice.edu/grads/publications/dom14.pdf
   */
  lazy val dominators: Array[(Int, Int)] = {
    val tree = dfst
    val bs = bounds
    val rpo = tree.postorder.reverse
    val rpons = rpo map (_.n)
    val rpobs = rpo map (_.b)
    val doms = new Array[(Int, Int)](tree.size)
    doms(0) = bs(0)
    var c = true
    //magick
    //this subfunction required quite a bit deviation from the pseudocode
    def intersect(b1: (Int, Int), b2: (Int, Int)): (Int, Int) = {
      var f1 = rpobs indexOf b1
      var f2 = rpobs indexOf b2
      var lf = -1
      while (f1 != f2) {
	while (f1 < f2) {
	  val f = if (doms(f1) == null) 0 else rpobs indexOf doms(f1)
	  f1 = if (lf == f1) f2 else f
	  lf = f
	}
	lf = -1
	while (f2 < f1) {
	  val f = if (doms(f2) == null) 0 else rpobs indexOf doms(f2)
	  f2 = if (lf == f2) f1 else f
	  lf = f
	}
	lf = -1
      }
      bs(rpons(f1))
    }
    while (c) {
      c = false
      val proc = new Array[Boolean](tree.size)
      proc(0) = true
      for (n <- rpo.init) {
	proc(n.n) = true
	val preds = predecessors(n.b)
	var idom = preds find (p => proc(bs indexOf p)) getOrElse {
	  if (preds.length == 0) {
	    val tc = method.tryCatches.find(_._3 == n.b._1).get
	    bs.find(_._1 == tc._1).get
	  } else preds(0)
	}
	for (p <- preds if !(p equals idom) && doms(bs indexOf p) != null) {
	  idom = intersect(p, idom)
	}
	if (doms(n.n) != idom) {
	  doms(n.n) = idom
	  c = true
	}
      }
    }
    doms
  }

  lazy val dominatedBy: Array[BitSet] = {
    val subs = Array.fill(bounds.length)(BitSet.empty)
    for (i <- 0 until bounds.length)
      subs(bounds indexOf dominators(i)) += i
    subs
  }

  lazy val dominanceFrontiers: Array[BitSet] = {
    val dfs = Array.fill(bounds.length)(BitSet.empty)
    for (i <- 0 until bounds.length) {
      val preds = predecessors(bounds(i))
      if (preds.length > 1)
	for (p <- preds) {
	  var run = p
	  while (run != dominators(i)) {
	    val j = bounds indexOf run
	    dfs(j) += i
	    run = dominators(if (dominators(j) == run) i else j)
	  }
	}
    }
    dfs
  }

  lazy val dominanceLostBy: Array[BitSet] = {
    val x = Array.fill(bounds.length)(BitSet.empty)
    for (i <- 0 until bounds.length) {
      val df = dominanceFrontiers(i)
      df foreach (idx => x(idx) += i)
    }
    x
  }
}
