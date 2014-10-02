/*Facile bytecode analysis, disassembling, and manipulation in Scala.
 *Copyright (C) 2013  David Phillips
 *
 *This program is free software: you can redistribute it and/or modify
 *it under the terms of the GNU General Public License as published by
 *the Free Software Foundation, either version 3 of the License, or
 *(at your option) any later version.
 *
 *This program is distributed in the hope that it will be useful,
 *but WITHOUT ANY WARRANTY; without even the implied warranty of
 *MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *GNU General Public License for more details.
 *
 *You should have received a copy of the GNU General Public License
 *along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package scala.bytecode

import org.objectweb.asm.{ClassReader, Opcodes}
import org.objectweb.asm.tree.ClassNode
import scala.collection.immutable.{ListMap => IMap}
import scala.collection.mutable.{HashMap => MMap, HashSet => MSet}

import java.io.{InputStream, File, FileInputStream}
import java.util.jar.JarFile

class Cxt {
  type DefinedOrUndefined = Either[ClassInfo, ClassInfo]
  import scala.{Right => Defined, Left => Undefined}
  /* Resolved classes -- whether we have it or not.
   * mmap(package -> mmap(className -> optional classInfo))
   */
  val pkgClasses: MMap[String, MMap[String, DefinedOrUndefined]] = MMap.empty
  val resourceStreams: MMap[String, InputStream] = MMap.empty

  def classesIn(pkg: String): MMap[String, DefinedOrUndefined] =
    pkgClasses.getOrElseUpdate(pkg, MMap.empty)

  def pkgs: List[Cxt.Pkg] = {
    val pnt = Cxt pkgNameTree pkgClasses.keys.toSeq
    def mkpkg(pkg: String): Cxt.Pkg =
      Cxt.Pkg(pkg, (pnt.getOrElse(pkg, Nil) map (sub =>
	mkpkg(pkg +'/'+ sub))).toList, classesIn(pkg).values.toList)
    val rootNames = pnt.keys filterNot (_ contains '/')
    (rootNames map mkpkg).toList
  }

  def lookup(qual: String): Option[ClassInfo] = {
    val (pkg, name) = Cxt pkgName qual
    (classesIn(pkg) get name) match {
      case Some(Defined(info)) => Some(info)
      case _ => None
    }
  }

  def diff(other: ClassInfo, info: ClassInfo) {}//this would be cool but how do?

  def resolve(node: ClassNode): ClassInfo = {
    val (pkg, name) = Cxt pkgName node.name
    val info = new ClassInfo(this, node)
    val classes = classesIn(pkg)
    (classes get name) match {
      case Some(Defined(oldInfo)) =>
	classes(name) = Defined(info)
      case _ =>
	classes(name) = Defined(info)
    }
    info
  }

  def resolve(qual: String): ClassInfo = {
    val (pkg, name) = Cxt pkgName qual
    val classes = classesIn(pkg)
    (classes get name) match {
      case Some(Defined(info)) => info
      case Some(Undefined(info)) => info
      case None =>
	val node = new ClassNode
	node.name = name
	val info = new ClassInfo(this, node)
	classes(name) = Undefined(info)
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

  def resolveDir(dir: File): List[ClassInfo] =
    (dir.listFiles.toList map (file =>
      if (file.isDirectory) resolveDir(file)
      else if (file.getName endsWith ".class") resolve(file) :: Nil
      else Nil)).flatten

  import scala.collection.JavaConversions._
  def resolveJar(jf: JarFile): List[Either[InputStream, ClassInfo]] =
    (for (entry <- jf.entries) yield {
      val name = entry.getName
      if (name endsWith ".class") Right(resolve(jf.getInputStream(entry)))
      else {
	val stream = jf.getInputStream(entry)
	resourceStreams(name) = stream
	Left(stream)
      }
    } ).toList

  def classLoader = new ClassLoader {
    override def loadClass(name: String): Class[_] =
      lookup(Cxt qual name) match {
	case Some(info) =>
	  val bytes = info.bytes
	  defineClass(name, bytes, 0, bytes.length)
	case _ =>
	  try {
	    findSystemClass(name)
	  } catch {
	    case _: Throwable => getClass.getClassLoader loadClass name
	  }
      }

    override def getResourceAsStream(name: String): InputStream = {
      (resourceStreams get name) getOrElse super.getResourceAsStream(name)
    }
  }

  def commonDesc(descs: Seq[String]): String = {
    def f(desc0: String, desc1: String): String = {
      if (desc0 equals desc1) desc0
      else {
	desc0
      }
    }
    descs reduce f _
  }
}

object Cxt {
  case class Pkg(name: String,
		 subpkgs: List[Pkg],
		 classes: List[Either[ClassInfo, ClassInfo]])

  val default: Cxt = new Cxt

  import Opcodes._
  val classModifierAccess: Map[Symbol, Int] = IMap(
    'public     -> ACC_PUBLIC,     'final     -> ACC_FINAL,
    'super      -> ACC_SUPER,      'interface -> ACC_INTERFACE,
    'abstract   -> ACC_ABSTRACT,   'synthetic -> ACC_SYNTHETIC,
    'annotation -> ACC_ANNOTATION, 'enum      -> ACC_ENUM)
  val fieldModifierAccess: Map[Symbol, Int] = IMap(
    'public    -> ACC_PUBLIC,    'private   -> ACC_PRIVATE,
    'protected -> ACC_PROTECTED, 'static    -> ACC_STATIC,
    'final     -> ACC_FINAL,     'volatile  -> ACC_VOLATILE,
    'transient -> ACC_TRANSIENT, 'synthetic -> ACC_SYNTHETIC,
    'enum      -> ACC_ENUM)
  val methodModifierAccess: Map[Symbol, Int] = IMap(
    'public    -> ACC_PUBLIC,    'private      -> ACC_PRIVATE,
    'protected -> ACC_PROTECTED, 'static       -> ACC_STATIC,
    'final     -> ACC_FINAL,     'synchronized -> ACC_SYNCHRONIZED,
    'bridge    -> ACC_BRIDGE,    'varargs      -> ACC_VARARGS,
    'native    -> ACC_NATIVE,    'abstract     -> ACC_ABSTRACT,
    'strict    -> ACC_STRICT,    'synthetic    -> ACC_SYNTHETIC)

  def pkgNameTree(names: Seq[String]): MMap[String, MSet[String]] = {
    val pnt: MMap[String, MSet[String]] = MMap.empty
    names foreach { n =>
      val s = (n split '/').toList
      val t = (s zip (0 until s.length)) map { case (p, q) => (s take q) :+ p }
      t foreach {
	case name :: Nil =>
	  if (! (pnt contains name)) pnt(name) = MSet.empty
	case splitNames =>
	  val owner = splitNames.init mkString "/"
	  val name = splitNames.last
	  if (pnt contains owner) pnt(owner) += name
	  else pnt(owner) = MSet(name)
      }
    }
    pnt
  }

  def pkgName(qual: String): (String, String) = qual.lastIndexOf('/') match {
    case -1 => ("", qual)
    case ix => (qual.substring(0, ix), qual.substring(ix + 1))
  }

  def qual(unqual: String): String = unqual.replace('.', '/')
}
