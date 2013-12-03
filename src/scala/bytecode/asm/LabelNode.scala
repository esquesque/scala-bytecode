package scala.bytecode.asm

import org.objectweb.asm.Label
import org.objectweb.asm.tree.{AbstractInsnNode => N, LabelNode => L}

object LabelNode {
  def apply(label: Label): L = new L(label)

  def unapply(n: N): Option[L] = n match {
    case l: L => Some(l); case _ => None
  }
}
