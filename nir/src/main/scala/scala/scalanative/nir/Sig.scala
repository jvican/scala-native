package scala.scalanative
package nir

final class Sig(val mangle: String) {
  final lazy val toProxy: Sig = {
    if (isMethod) {
      val Sig.Method(id, types) = this.unmangled
      Sig.Proxy(id, types.init).mangled
    } else {
      util.unsupported(
        s"can't convert non-method sig ${this.mangle} to proxy sig"
      )
    }
  }

  final def show: String             = Show(this)
  final def unmangled: Sig.Unmangled = Unmangle.unmangleSig(mangle)

  final def isField: Boolean     = mangle(0) == 'F'
  final def isCtor: Boolean      = mangle(0) == 'R'
  final def isMethod: Boolean    = mangle(0) == 'D'
  final def isProxy: Boolean     = mangle(0) == 'P'
  final def isExtern: Boolean    = mangle(0) == 'C'
  final def isGenerated: Boolean = mangle(0) == 'G'
  final def isDuplicate: Boolean = mangle(0) == 'K'

  final override def equals(other: Any): Boolean =
    (this eq other.asInstanceOf[AnyRef]) || (other match {
      case other: Sig => other.mangle == mangle
      case _          => false
    })
  final override lazy val hashCode: Int =
    mangle.##
  final override def toString: String =
    mangle
}
object Sig {
  sealed abstract class Unmangled {
    final def mangled: Sig = new Sig(Mangle(this))
  }

  final case class Field(id: String)                    extends Unmangled
  final case class Ctor(types: Seq[Type])               extends Unmangled
  final case class Method(id: String, types: Seq[Type]) extends Unmangled
  final case class Proxy(id: String, types: Seq[Type])  extends Unmangled
  final case class Extern(id: String)                   extends Unmangled
  final case class Generated(id: String)                extends Unmangled
  final case class Duplicate(of: Sig, types: Seq[Type]) extends Unmangled

  import scala.language.implicitConversions
  implicit def unmangledToMangled(sig: Sig.Unmangled): Sig = sig.mangled
}
