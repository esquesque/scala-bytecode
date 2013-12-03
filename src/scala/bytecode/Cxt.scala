package scala.bytecode

import org.objectweb.asm.{ClassReader, Opcodes}
import org.objectweb.asm.tree.ClassNode
import scala.collection.immutable.{ListMap => IMap}
import scala.collection.mutable.{HashMap => MMap}

import java.io.{InputStream, File, FileInputStream}

class Cxt {
  val pkgClasses: MMap[String, MMap[String, ClassInfo]] = MMap.empty

  def diff(other: ClassInfo, info: ClassInfo) {}

  def resolve(node: ClassNode): ClassInfo = {
    val name = Cxt.name(node.name)
    val pkg = Cxt.pkg(node.name)
    val info = new ClassInfo(this, node)
    pkgClasses.get(pkg) match {
      case Some(classes) =>
	if (classes contains name) diff(classes(name), info)
        classes(name) = info
      case None => pkgClasses(pkg) = MMap(name -> info)
    }
    info
  }

  def resolve(qual: String): ClassInfo = {
    val name = Cxt.name(qual)
    val pkg = Cxt.pkg(qual)
    pkgClasses.get(pkg) match {
      case Some(classes) =>
	classes.getOrElseUpdate(name, {
	  val node = new ClassNode
	  node.name = name
	  new ClassInfo(this, node)
	} )
      case None =>
	val node = new ClassNode
	node.name = name
	val info = new ClassInfo(this, node)
	pkgClasses(pkg) = MMap(name -> info)
	info
    }
  }

  import ClassReader.{SKIP_DEBUG, SKIP_FRAMES}
  private val flags = SKIP_DEBUG | SKIP_FRAMES
  def resolve(bytes: Array[Byte]): ClassInfo = {
    val cn = new ClassNode
    val cr = new ClassReader(bytes)
    cr.accept(cn, flags)
    resolve(cn)
  }

  def resolve(is: InputStream): ClassInfo = {
    val cn = new ClassNode
    val cr = new ClassReader(is)
    cr.accept(cn, flags)
    resolve(cn)
  }

  def resolve(file: File): ClassInfo = resolve(new FileInputStream(file))
}

object Cxt {
  val default: Cxt = new Cxt

  import Opcodes._
  val classModifierAccess: Map[Symbol, Int] = IMap(
    'public     -> ACC_PUBLIC,     'final     -> ACC_FINAL,
    'super      -> ACC_SUPER,      'interface -> ACC_INTERFACE,
    'abstract   -> ACC_ABSTRACT,   'synthetic -> ACC_SYNTHETIC,
    'annotation -> ACC_ANNOTATION, 'enum      -> ACC_ENUM)
  val methodModifierAccess: Map[Symbol, Int] = IMap(
    'public    -> ACC_PUBLIC,    'private      -> ACC_PRIVATE,
    'protected -> ACC_PROTECTED, 'static       -> ACC_STATIC,
    'final     -> ACC_FINAL,     'synchronized -> ACC_SYNCHRONIZED,
    'bridge    -> ACC_BRIDGE,    'varargs      -> ACC_VARARGS,
    'native    -> ACC_NATIVE,    'abstract     -> ACC_ABSTRACT,
    'strict    -> ACC_STRICT,    'synthetic    -> ACC_SYNTHETIC)

  def pkg(qual: String): String = qual.lastIndexOf('/') match {
    case -1 => ""
    case ix => qual.substring(0, ix)
  }

  def name(qual: String): String = qual.lastIndexOf('/') match {
    case -1 => qual
    case ix => qual.substring(ix + 1)
  }
}
