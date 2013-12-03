package scala.bytecode.asm

import org.objectweb.asm.Label
import org.objectweb.asm.tree.{JumpInsnNode => J,
			       AbstractInsnNode => N,
			       LabelNode => L}

object JumpInsnNode {
  def apply(opcode: Int, label: L): J = new J(opcode, label)

  def unapply(n: N): Option[(Int, L)] = n match {
    case j: J => Some(j.getOpcode, j.label); case _ => None
  }
}
