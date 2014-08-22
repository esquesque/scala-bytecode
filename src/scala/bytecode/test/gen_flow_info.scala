package scala.bytecode.test

import scala.bytecode._
import scala.bytecode.ast.Block

object gen_flow_info {
  def main(args: Array[String]) {
/*    val cxt = Cxt.default
    val x = cxt.resolve(new java.io.File("scratch/simple_ifs.class"))
    def abbrBlocks(bs: List[Block]): String =
      bs.map(_.ordinal).mkString("[", ", ", "]")
    for (method <- x.methods) {
      println(method)
      val cfga = method.cfgAnalyzer(MethodInfo.basicInterpreter)
      val frames = cfga.analyze(method.owner.name, method.node)
      val cfg = method.cfg
      val blocks = cfg.mkblocks(frames)
      val head = blocks.head
      println("block_0="+ head)
      println(" succs="+ abbrBlocks(head.successors))
      println(" subs="+ abbrBlocks(head.dominated))
      for ((block, idx) <- blocks.zipWithIndex.tail) {
	println("block_"+ idx +"="+ block)
	println(" succs="+ abbrBlocks(block.successors))
	println(" subs="+ abbrBlocks(block.dominated))
	println(" dom_exiters=: "+ abbrBlocks(block.dominanceLost))
      }
      println()
    }*/
  }
}
