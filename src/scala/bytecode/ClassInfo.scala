package scala.bytecode

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.tree.{ClassNode, FieldNode, MethodNode}

class ClassInfo(val cxt: Cxt, val node: ClassNode)
extends Info[ClassNode, ast.ClassDecl] {
  lazy val tree = ast.ClassDecl(this)
  lazy val superclass: ClassInfo = cxt resolve superName

  val modifiers: List[Symbol] = Cxt.classModifierAccess.toList map {
    case (sym, acc) => if ((acc & node.access) != 0) Some(sym) else None
  } filterNot (_.isEmpty) map (_.get)

  val name = node.name
  val desc = 'L' + name + ';'
  val verbose = name

  val superName = if (node.superName == null) "java/lang/Object"
		  else node.superName

  import java.util.{List => list}
  def newMethod(modifiers: Symbol*)(name: String, desc: String,
				    insnList: asm.RichInsnList): MethodInfo = {
    val method = MethodInfo(modifiers: _*)(name, desc, insnList)(this)
    node.methods.asInstanceOf[list[MethodNode]].add(method.node)
    method
  }

  import scala.collection.JavaConversions._
  def interfaces: List[String] =
    node.interfaces.asInstanceOf[list[String]].toList
  def fields: List[FieldInfo] =
    node.fields.asInstanceOf[list[FieldNode]].toList map (
      new FieldInfo(cxt, this, _))
  def methods: List[MethodInfo] =
    node.methods.asInstanceOf[list[MethodNode]].toList map (
      new MethodInfo(cxt, this, _))

  def bytes: Array[Byte] = {
    val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    node.accept(cw)
    cw.toByteArray
  }

  /*def dump(writer: java.io.Writer, indent: Int) {
    writer append " "*indent
    if (modifiers.nonEmpty) {
      writer append (modifiers filter ('interface !=) map (_.name)).mkString(" ")
      writer append " "
    }
    writer append (if (this is 'interface) "interface " else "class ")
    writer append name
    writer append " extends "
    writer append node.superName
    interfaces match {
      case x if x.isEmpty =>
      case x => writer append x.mkString(
	(if (this is 'interface) ", " else " implements "), ", ", "")
    }
    writer append " {\n"
    fields foreach (_.dump(writer, indent + 2))
    methods foreach (_.dump(writer, indent + 2))
    writer append "}\n"
    writer.flush
  }*/
}

object ClassInfo {
  val anonymous: ClassInfo = ClassInfo('public, 'final)("anon")(Cxt.default)

  def apply(modifiers: Symbol*)(name: String,
				superName: String = "java/lang/Object")(
	implicit cxt: Cxt): ClassInfo = {
    val node = new ClassNode
    node.access = (modifiers map Cxt.classModifierAccess) reduce (_ | _)
    node.name = name
    node.superName = superName
    cxt.resolve(node)
  }
}
