package scala.bytecode.asm

import org.objectweb.asm.tree.{IntInsnNode => I, AbstractInsnNode => N}

object IntInsnNode {
  def apply(opcode: Int, operand: Int): I = new I(opcode, operand)

  def unapply(n: N): Option[(Int, Int)] = n match {
    case i: I => Some(i.getOpcode, i.operand); case _ => None
  }
}
