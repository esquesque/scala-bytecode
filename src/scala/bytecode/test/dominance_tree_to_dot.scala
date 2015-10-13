package scala.bytecode.test

import java.io._
import scala.bytecode._

object dominance_tree_to_dot {
  def main(args: Array[String]) {
    val cases = ifs.cases ++ loops.cases
    val transforms = CollapseTernaryExprs :: AnchorFloatingStmts :: Nil
    var a = 0
    for (c <- cases) {
      a += 1
      val method = c.method
      transforms foreach (method apply _)
      val n = method.name
      val f = new File(args(0) +"/"+ n +".dot")
      val dot = new PrintWriter(f)
      dot append ("digraph "+ a +" {\n  ")
      val blocks = method.tree.blocks
      for (b <- blocks)
	dot append ("block_"+ b.ordinal +" [label=\"#"+ b.ordinal +"\"];\n  ")
      for (b <- blocks) {
	for (x <- b.immediatelyDominated)
	  dot append ("block_"+ b.ordinal +" -> block_"+ x.ordinal +";\n  ")
	for (z <- b.immediatelyPostdominated)
	  dot append ("block_"+ z.ordinal +" -> block_"+ b.ordinal +" [style=dotted];\n  ")
      }
      dot append "\n}\n"
      dot.flush
      dot.close
    }
  }
}
