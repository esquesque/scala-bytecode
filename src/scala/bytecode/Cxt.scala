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
  val pkgClasses: MMap[String, MMap[String, ClassInfo]] = MMap.empty
  val resourceStreams: MMap[String, InputStream] = MMap.empty

  def classes(pkgName: String): List[ClassInfo] =
    pkgClasses.getOrElse(pkgName, MMap.empty).values.toList

  def pkgs: List[Cxt.Pkg] = {
    val pnt = Cxt.pkgNameTree(pkgClasses.keys.toSeq)
    def mkpkg(name: String): Cxt.Pkg =
      Cxt.Pkg(name, (pnt.getOrElse(name, Nil) map (sub =>
	mkpkg(name +'/'+ sub))).toList, classes(name))
    val rootNames = pnt.keys filterNot (_ contains '/')
    (rootNames map mkpkg).toList
  }

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

  def resolveDir(dir: File): List[ClassInfo] =
    (dir.listFiles.toList map (file =>
      if (file.isDirectory) resolveDir(file)
      else if (file.getName endsWith ".class") resolve(file) :: Nil
      else Nil)).flatten

  import scala.collection.JavaConversions._
  def resolveJar(jf: java.util.jar.JarFile): List[Either[ClassInfo, InputStream]] =
    (for (entry <- jf.entries) yield {
      val name = entry.getName
      if (name endsWith ".class") Left(resolve(jf.getInputStream(entry)))
      else {
	val stream = jf.getInputStream(entry)
	resourceStreams(name) = stream
	Right(stream)
      }
    } ).toList
}

object Cxt {
  case class Pkg(name: String, subpkgs: List[Pkg], classes: List[ClassInfo])

  val default: Cxt = new Cxt

  import Opcodes._
  val classModifierAccess: Map[Symbol, Int] = IMap(
    'public     -> ACC_PUBLIC,     'final     -> ACC_FINAL,
    'super      -> ACC_SUPER,      'interface -> ACC_INTERFACE,
    'abstract   -> ACC_ABSTRACT,   'synthetic -> ACC_SYNTHETIC,
    'annotation -> ACC_ANNOTATION, 'enum      -> ACC_ENUM)
  val fieldModifierAccess: Map[Symbol, Int] = IMap(
    'public    -> ACC_PUBLIC,
    'private   -> ACC_PRIVATE,
    'protected -> ACC_PROTECTED,
    'static    -> ACC_STATIC,
    'final     -> ACC_FINAL,
    'volatile  -> ACC_VOLATILE,
    'transient -> ACC_TRANSIENT,
    'synthetic -> ACC_SYNTHETIC,
    'enum -> ACC_ENUM)
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

  def pkg(qual: String): String = qual.lastIndexOf('/') match {
    case -1 => ""
    case ix => qual.substring(0, ix)
  }

  def name(qual: String): String = qual.lastIndexOf('/') match {
    case -1 => qual
    case ix => qual.substring(ix + 1)
  }
}
