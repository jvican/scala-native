package scala.scalanative
package nir

sealed abstract class Global {
  def top: Global.Top
  def member(sig: Sig): Global.Member
  def member(sig: Sig.Unmangled): Global.Member = member(sig.mangled)

  final def isTop: Boolean =
    this.isInstanceOf[Global.Top]
  final def show: String =
    Show(this)
  final def mangle: String =
    Mangle(this)
}
trait CacheHashCode { self: Product =>
  // Cache hash code here so that `Dag.toDotGraph` doesn't recompute it all the time
  override lazy val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(self)
}

object Global {
  final case object None extends Global {
    override def top: Global.Top =
      throw new Exception("None doesn't have a top.")
    override def member(sig: Sig) =
      throw new Exception("Global.None can't have any members.")
  }

  final case class Top(val id: String) extends Global {
    override def top: Global.Top =
      this
    override def member(sig: Sig): Global.Member =
      Global.Member(this, sig)
  }

  final case class Member(val owner: Global, val sig: Sig) extends Global {
    override def top: Global.Top =
      owner.top
    override def member(sig: Sig): Global.Member =
      throw new Exception("Global.Member can't have any members.")
  }
}
