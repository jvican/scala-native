package scala.scalanative
package linker

import scala.collection.mutable
import scalanative.nir._

import java.util.{HashSet, HashMap}
import scala.collection.JavaConverters._

sealed abstract class Info {
  def attrs: Attrs
  def name: Global
}

sealed abstract class ScopeInfo extends Info {
  val members = mutable.UnrolledBuffer.empty[MemberInfo]
  val calls   = new HashSet[Sig]

  def isClass: Boolean = this.isInstanceOf[Class]
  def isTrait: Boolean = this.isInstanceOf[Class]
  def is(info: ScopeInfo): Boolean
  def targets(sig: Sig): HashSet[Global]
  def implementors: HashSet[Class]
}

sealed abstract class MemberInfo extends Info {
  def owner: Info
}

final class Unavailable(val name: Global) extends Info {
  def attrs: Attrs =
    util.unsupported(s"unavailable ${name.show} has no attrs")
}

final class Trait(val attrs: Attrs, val name: Global, val traits: Seq[Trait])
    extends ScopeInfo {
  val implementors = new HashSet[Class]()
  val subtraits    = new HashSet[Trait]()

  def targets(sig: Sig): HashSet[Global] = {
    val out = new HashSet[Global]()

    val it = implementors.iterator
    while (it.hasNext) {
      val cls = it.next
      if (cls.allocated) {
        cls.resolve(sig) match {
          case Some(impl) => out.add(impl)
          case None       => ()
        }
      }
    }

    out
  }

  def is(info: ScopeInfo): Boolean = (info eq this) || {
    info match {
      case info: Trait =>
        info.subtraits.contains(this)
      case _ =>
        false
    }
  }
}

final class Class(
    val attrs: Attrs,
    val name: Global,
    val parent: Option[Class],
    val traits: Seq[Trait],
    val isModule: Boolean
) extends ScopeInfo {
  var allocated  = false
  val subclasses = new HashSet[Class]()
  val responds   = new HashMap[Sig, Global]()
  val implementors = {
    val initial = new HashSet[Class]()
    initial.add(this)
    initial
  }

  lazy val fields: Seq[Field] = {
    val out = mutable.UnrolledBuffer.empty[Field]
    def add(info: Class): Unit =
      info.members.foreach {
        case info: Field => out += info
        case _           => ()
      }
    parent.foreach(add)
    add(this)
    out
  }

  val ty: Type =
    Type.Ref(name)
  def isStaticModule(implicit top: Result): Boolean =
    isModule && !top.infos.contains(name.member(Sig.Ctor(Seq())))
  def resolve(sig: Sig): Option[Global] =
    Option(responds.get(sig))

  def targets(sig: Sig): HashSet[Global] = {
    val out = new HashSet[Global]

    def add(cls: Class): Unit =
      if (cls.allocated) {
        cls.resolve(sig) match {
          case Some(impl) => out.add(impl)
          case None       => ()
        }
      }

    add(this)
    val it = subclasses.iterator
    while (it.hasNext) {
      add(it.next)
    }

    out
  }

  def is(info: ScopeInfo): Boolean = (info eq this) || {
    info match {
      case info: Trait =>
        info.implementors.contains(this)
      case info: Class =>
        info.subclasses.contains(this)
      case _ =>
        false
    }
  }
}

final class Method(
    val attrs: Attrs,
    val owner: Info,
    val name: Global,
    val ty: Type,
    val insts: Array[Inst]
) extends MemberInfo {
  val value: Val =
    if (isConcrete) {
      Val.Global(name, Type.Ptr)
    } else {
      Val.Null
    }
  def isConcrete: Boolean =
    insts.nonEmpty
}

final class Field(
    val attrs: Attrs,
    val owner: Info,
    val name: Global,
    val isConst: Boolean,
    val ty: nir.Type,
    val init: Val
) extends MemberInfo {
  lazy val index: Int =
    owner.asInstanceOf[Class].fields.indexOf(this)
}

final class Result(
    val infos: mutable.Map[Global, Info],
    val entries: Seq[Global],
    val unavailable: Seq[Global],
    val links: Seq[Attr.Link],
    val defns: Seq[Defn],
    val dynsigs: Seq[Sig],
    val dynimpls: Seq[Global]
) {
  lazy val StringClass       = infos(Rt.StringName).asInstanceOf[Class]
  lazy val StringValueField  = infos(Rt.StringValueName).asInstanceOf[Field]
  lazy val StringOffsetField = infos(Rt.StringOffsetName).asInstanceOf[Field]
  lazy val StringCountField  = infos(Rt.StringCountName).asInstanceOf[Field]
  lazy val StringCachedHashCodeField = infos(Rt.StringCachedHashCodeName)
    .asInstanceOf[Field]
}
