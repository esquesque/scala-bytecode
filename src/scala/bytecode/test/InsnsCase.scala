package scala.bytecode.test

trait InsnsCase extends Case[List[org.objectweb.asm.tree.AbstractInsnNode]] {
  def apply: Boolean = test(method.instructions.toList)
}
