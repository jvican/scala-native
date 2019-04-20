package scala.scalanative
package nir
package serialization

import java.nio.ByteBuffer
import scala.collection.mutable
import scalanative.nir.serialization.{Tags => T}

import scala.scalanative.util.Scope

final class BinaryDeserializer(buffer: ByteBuffer, scope: Scope) {
  import buffer._

  private val header: List[(Global, Int)] = {
    buffer.position(0)

    val magic    = getInt
    val compat   = getInt
    val revision = getInt

    assert(magic == Versions.magic, "Can't read non-NIR file.")
    assert(
      compat == Versions.compat && revision <= Versions.revision,
      "Can't read binary-incompatible version of NIR."
    )

    var i: Int                   = 1
    val end                      = getInt
    var seq: List[(Global, Int)] = Nil
    while (i <= end) {
      seq = (getGlobal, getInt) :: seq
      i += 1
    }
    seq
  }

  //final def globals: Set[Global] = header.keySet

  final def deserialize(): Seq[Defn] = {
    var allDefns = List.empty[Defn]
    header.foreach {
      case (g, offset) =>
        buffer.position(offset)
        allDefns ::= getDefn
    }
    scope.close()
    allDefns
  }

  private def getSeq[T](getT: => T): Seq[T] = {
    var i: Int       = 1
    val end          = getInt
    var seq: List[T] = Nil
    while (i <= end) {
      seq = getT :: seq
      i += 1
    }
    seq.reverse
  }

  private def getOpt[T](getT: => T): Option[T] =
    if (get == 0) None else Some(getT)

  private def getInts(): Seq[Int] = {
    var i: Int         = 1
    val end            = getInt
    var seq: List[Int] = Nil
    while (i <= end) {
      seq = getInt :: seq
      i += 1
    }
    seq.reverse
  }

  private def getStrings(): Seq[String] = {
    var i: Int            = 1
    val end               = getInt
    var seq: List[String] = Nil
    while (i <= end) {
      seq = getString :: seq
      i += 1
    }
    seq.reverse
  }

  private def getString(): String = {
    import scala.scalanative.io.internedStrings

    val length = getInt
    val arr = {
      if (length <= 128) {
        buffer.get(BinaryDeserializer.sharedArr128, 0, length)
        BinaryDeserializer.sharedArr128
      } else if (length <= 256) {
        buffer.get(BinaryDeserializer.sharedArr256, 0, length)
        BinaryDeserializer.sharedArr256
      } else if (length <= 512) {
        buffer.get(BinaryDeserializer.sharedArr512, 0, length)
        BinaryDeserializer.sharedArr512
      } else {
        buffer.get(BinaryDeserializer.sharedArr4096, 0, length)
        BinaryDeserializer.sharedArr4096
      }
    }

    val string = new String(arr, 0, length, "UTF-8")

    val ref = internedStrings.get(string)
    if (ref == null) {
      val ref = new java.lang.ref.WeakReference(string)
      internedStrings.put(string, ref)
      string
    } else {
      ref.get()
    }
  }

  private def getBool(): Boolean = get != 0

  private def getAttrs(): Attrs = {
    // Inlined because overhead of creating sequence + using `Attrs.fromSeq` is high
    import scala.scalanative.nir.Attr._
    var inline     = Attrs.None.inline
    var specialize = Attrs.None.specialize
    var opt        = Attrs.None.opt
    var isExtern   = false
    var isDyn      = false
    var isStub     = false
    var isAbstract = false
    var links      = List.empty[Attr.Link]

    var i: Int          = 1
    val end             = getInt
    var seq: List[Attr] = Nil
    while (i <= end) {
      getAttr match {
        case attr: Inline     => inline = attr
        case attr: Specialize => specialize = attr
        case attr: Opt        => opt = attr
        case Extern           => isExtern = true
        case Dyn              => isDyn = true
        case Stub             => isStub = true
        case link: Attr.Link  => links ::= link
        case Abstract         => isAbstract = true
      }
      i += 1
    }

    new Attrs(inline, specialize, opt, isExtern, isDyn, isStub, isAbstract, links)
  }

  private def getAttr(): Attr = getInt match {
    case T.MayInlineAttr    => Attr.MayInline
    case T.InlineHintAttr   => Attr.InlineHint
    case T.NoInlineAttr     => Attr.NoInline
    case T.AlwaysInlineAttr => Attr.AlwaysInline

    case T.MaySpecialize => Attr.MaySpecialize
    case T.NoSpecialize  => Attr.NoSpecialize

    case T.UnOptAttr   => Attr.UnOpt
    case T.NoOptAttr   => Attr.NoOpt
    case T.DidOptAttr  => Attr.DidOpt
    case T.BailOptAttr => Attr.BailOpt(getString)

    case T.DynAttr      => Attr.Dyn
    case T.StubAttr     => Attr.Stub
    case T.ExternAttr   => Attr.Extern
    case T.LinkAttr     => Attr.Link(getString)
    case T.AbstractAttr => Attr.Abstract
  }

  private def getBin(): Bin = getInt match {
    case T.IaddBin => Bin.Iadd
    case T.FaddBin => Bin.Fadd
    case T.IsubBin => Bin.Isub
    case T.FsubBin => Bin.Fsub
    case T.ImulBin => Bin.Imul
    case T.FmulBin => Bin.Fmul
    case T.SdivBin => Bin.Sdiv
    case T.UdivBin => Bin.Udiv
    case T.FdivBin => Bin.Fdiv
    case T.SremBin => Bin.Srem
    case T.UremBin => Bin.Urem
    case T.FremBin => Bin.Frem
    case T.ShlBin  => Bin.Shl
    case T.LshrBin => Bin.Lshr
    case T.AshrBin => Bin.Ashr
    case T.AndBin  => Bin.And
    case T.OrBin   => Bin.Or
    case T.XorBin  => Bin.Xor
  }

  private def getInsts(): Seq[Inst] = getSeq(getInst)
  private def getInst(): Inst = getInt match {
    case T.LabelInst       => Inst.Label(getLocal, getParams)
    case T.LetInst         => Inst.Let(getLocal, getOp, Next.None)
    case T.LetUnwindInst   => Inst.Let(getLocal, getOp, getNext)
    case T.RetInst         => Inst.Ret(getVal)
    case T.JumpInst        => Inst.Jump(getNext)
    case T.IfInst          => Inst.If(getVal, getNext, getNext)
    case T.SwitchInst      => Inst.Switch(getVal, getNext, getNexts)
    case T.ThrowInst       => Inst.Throw(getVal, getNext)
    case T.UnreachableInst => Inst.Unreachable(getNext)
  }

  private def getComp(): Comp = getInt match {
    case T.IeqComp => Comp.Ieq
    case T.IneComp => Comp.Ine
    case T.UgtComp => Comp.Ugt
    case T.UgeComp => Comp.Uge
    case T.UltComp => Comp.Ult
    case T.UleComp => Comp.Ule
    case T.SgtComp => Comp.Sgt
    case T.SgeComp => Comp.Sge
    case T.SltComp => Comp.Slt
    case T.SleComp => Comp.Sle

    case T.FeqComp => Comp.Feq
    case T.FneComp => Comp.Fne
    case T.FgtComp => Comp.Fgt
    case T.FgeComp => Comp.Fge
    case T.FltComp => Comp.Flt
    case T.FleComp => Comp.Fle
  }

  private def getConv(): Conv = getInt match {
    case T.TruncConv    => Conv.Trunc
    case T.ZextConv     => Conv.Zext
    case T.SextConv     => Conv.Sext
    case T.FptruncConv  => Conv.Fptrunc
    case T.FpextConv    => Conv.Fpext
    case T.FptouiConv   => Conv.Fptoui
    case T.FptosiConv   => Conv.Fptosi
    case T.UitofpConv   => Conv.Uitofp
    case T.SitofpConv   => Conv.Sitofp
    case T.PtrtointConv => Conv.Ptrtoint
    case T.InttoptrConv => Conv.Inttoptr
    case T.BitcastConv  => Conv.Bitcast
  }

  private def getDefns(): Seq[Defn] = getSeq(getDefn)
  private def getDefn(): Defn = getInt match {
    case T.VarDefn =>
      Defn.Var(getAttrs, getGlobal, getType, getVal)

    case T.ConstDefn =>
      Defn.Const(getAttrs, getGlobal, getType, getVal)

    case T.DeclareDefn =>
      Defn.Declare(getAttrs, getGlobal, getType)

    case T.DefineDefn =>
      Defn.Define(getAttrs, getGlobal, getType, getInsts)

    case T.TraitDefn =>
      Defn.Trait(getAttrs, getGlobal, getGlobals)

    case T.ClassDefn =>
      Defn.Class(getAttrs, getGlobal, getGlobalOpt, getGlobals)

    case T.ModuleDefn =>
      Defn.Module(getAttrs, getGlobal, getGlobalOpt, getGlobals)
  }

  private def getGlobals(): Seq[Global]      = getSeq(getGlobal)
  private def getGlobalOpt(): Option[Global] = getOpt(getGlobal)
  private def getGlobal(): Global = {
    getInt match {
      case T.NoneGlobal => Global.None
      case g =>
        val top = Global.Top(getString)
        g match {
          case T.TopGlobal    => top
          case T.MemberGlobal => Global.Member(top, getSig)
        }
    }
  }

  private def getSig(): Sig =
    new Sig(getString)

  private def getLocal(): Local =
    Local(getLong)

  private def getNexts(): Seq[Next] = getSeq(getNext)
  private def getNext(): Next = getInt match {
    case T.NoneNext   => Next.None
    case T.UnwindNext => Next.Unwind(getParam, getNext)
    case T.CaseNext   => Next.Case(getVal, getNext)
    case T.LabelNext  => Next.Label(getLocal, getVals)
  }

  private def getOp(): Op = getInt match {
    case T.CallOp       => Op.Call(getType, getVal, getVals)
    case T.LoadOp       => Op.Load(getType, getVal)
    case T.StoreOp      => Op.Store(getType, getVal, getVal)
    case T.ElemOp       => Op.Elem(getType, getVal, getVals)
    case T.ExtractOp    => Op.Extract(getVal, getInts)
    case T.InsertOp     => Op.Insert(getVal, getVal, getInts)
    case T.StackallocOp => Op.Stackalloc(getType, getVal)
    case T.BinOp        => Op.Bin(getBin, getType, getVal, getVal)
    case T.CompOp       => Op.Comp(getComp, getType, getVal, getVal)
    case T.ConvOp       => Op.Conv(getConv, getType, getVal)

    case T.ClassallocOp  => Op.Classalloc(getGlobal)
    case T.FieldloadOp   => Op.Fieldload(getType, getVal, getGlobal)
    case T.FieldstoreOp  => Op.Fieldstore(getType, getVal, getGlobal, getVal)
    case T.MethodOp      => Op.Method(getVal, getSig)
    case T.DynmethodOp   => Op.Dynmethod(getVal, getSig)
    case T.ModuleOp      => Op.Module(getGlobal)
    case T.AsOp          => Op.As(getType, getVal)
    case T.IsOp          => Op.Is(getType, getVal)
    case T.CopyOp        => Op.Copy(getVal)
    case T.SizeofOp      => Op.Sizeof(getType)
    case T.BoxOp         => Op.Box(getType, getVal)
    case T.UnboxOp       => Op.Unbox(getType, getVal)
    case T.VarOp         => Op.Var(getType)
    case T.VarloadOp     => Op.Varload(getVal)
    case T.VarstoreOp    => Op.Varstore(getVal, getVal)
    case T.ArrayallocOp  => Op.Arrayalloc(getType, getVal)
    case T.ArrayloadOp   => Op.Arrayload(getType, getVal, getVal)
    case T.ArraystoreOp  => Op.Arraystore(getType, getVal, getVal, getVal)
    case T.ArraylengthOp => Op.Arraylength(getVal)
  }

  private def getParams(): Seq[Val.Local] = getSeq(getParam)
  private def getParam(): Val.Local       = Val.Local(getLocal, getType)

  private def getTypes(): Seq[Type] = getSeq(getType)
  private def getType(): Type = {
    val kind = getInt
    kind match {
      case T.RefType => Type.Ref(getGlobal, getBool, getBool)
      case otherKind =>
        otherKind match {
          case T.VarargType      => Type.Vararg
          case T.PtrType         => Type.Ptr
          case T.BoolType        => Type.Bool
          case T.CharType        => Type.Char
          case T.ByteType        => Type.Byte
          case T.ShortType       => Type.Short
          case T.IntType         => Type.Int
          case T.LongType        => Type.Long
          case T.FloatType       => Type.Float
          case T.DoubleType      => Type.Double
          case T.ArrayValueType  => Type.ArrayValue(getType, getInt)
          case T.StructValueType => Type.StructValue(getTypes)
          case T.FunctionType    => Type.Function(getTypes, getType)

          case T.NullType    => Type.Null
          case T.NothingType => Type.Nothing
          case T.VirtualType => Type.Virtual
          case T.VarType     => Type.Var(getType)
          case T.UnitType    => Type.Unit
          case T.ArrayType   => Type.Array(getType, getBool)
        }
    }
  }

  private def getVals(): Seq[Val] = getSeq(getVal)
  private def getVal(): Val = {
    val kind = getInt
    kind match {
      case T.LocalVal => Val.Local(getLocal, getType)
      case otherKind =>
        otherKind match {
          case T.TrueVal        => Val.True
          case T.FalseVal       => Val.False
          case T.NullVal        => Val.Null
          case T.ZeroVal        => Val.Zero(getType)
          case T.CharVal        => Val.Char(getShort.toChar)
          case T.ByteVal        => Val.Byte(get)
          case T.ShortVal       => Val.Short(getShort)
          case T.IntVal         => Val.Int(getInt)
          case T.LongVal        => Val.Long(getLong)
          case T.FloatVal       => Val.Float(getFloat)
          case T.DoubleVal      => Val.Double(getDouble)
          case T.StructValueVal => Val.StructValue(getVals)
          case T.ArrayValueVal  => Val.ArrayValue(getType, getVals)
          case T.CharsVal       => Val.Chars(getString)
          case T.GlobalVal      => Val.Global(getGlobal, getType)

          case T.UnitVal    => Val.Unit
          case T.ConstVal   => Val.Const(getVal)
          case T.StringVal  => Val.String(getString)
          case T.VirtualVal => Val.Virtual(getLong)
        }
    }
  }
}

object BinaryDeserializer {
  private[serialization] val sharedArr128  = new Array[Byte](128)
  private[serialization] val sharedArr256  = new Array[Byte](256)
  private[serialization] val sharedArr512  = new Array[Byte](512)
  private[serialization] val sharedArr4096 = new Array[Byte](4096)
}
