package scala.bytecode.asm

import org.objectweb.asm.{MethodVisitor, Opcodes}
import org.objectweb.asm.tree.{AbstractInsnNode => N,
			       LabelNode => L,
			       MethodInsnNode => M}
import Opcodes.{INVOKESTATIC => OP}

class InitNewNode(val instance: String,
		  owner: String,
		  desc: String) extends M(OP, owner, "<init>", desc)

object InitNewNode {
  def apply(instance: String, owner: String, desc: String): InitNewNode =
    new InitNewNode(instance, owner, desc)

  def unapply(n: N): Option[(String, String, String)] = n match {
    case inn: InitNewNode => Some((inn.instance, inn.owner, inn.desc))
    case _ => None
  }
}
