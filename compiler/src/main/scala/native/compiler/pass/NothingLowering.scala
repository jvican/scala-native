package native
package compiler
package pass

import scala.collection.mutable
import scala.util.control.Breaks._
import native.nir._
import native.util.unsupported

/** Eliminates:
 *  - Type.Nothing
 */
class NothingLowering extends Pass {
  override def preBlock = { case Block(n, params, insts) =>
    val ninsts = mutable.UnrolledBuffer.empty[Inst]
    breakable {
      insts.foreach {
        case inst if inst.op.resty != Type.Nothing =>
          ninsts += inst
        case Inst(_, call: Op.Call) if call.resty == Type.Nothing =>
          ninsts += Inst(call)
          ninsts += Inst(Op.Unreachable)
          break
        case inst @ Inst(_, termn: Op.Cf) =>
          ninsts += inst
          break
      }
    }
    Seq(Block(n, params, ninsts.toSeq))
  }

  override def preType = {
    case Type.Nothing =>
      unsupported("nothing can only be used as the result type of the function")
    case Type.Function(params, Type.Nothing) =>
      Type.Function(params, Type.Nothing)
  }
}
