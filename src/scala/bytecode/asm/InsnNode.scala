package scala.bytecode.asm

import org.objectweb.asm.tree.{InsnNode => I, AbstractInsnNode => N}

object InsnNode {
  def apply(opcode: Int): I = new I(opcode)

  def unapply(n: N): Option[Int] =
    if (n.getType == N.INSN) Some(n.getOpcode) else None
}
