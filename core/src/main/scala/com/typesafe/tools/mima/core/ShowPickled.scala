package com.typesafe.tools.mima.core

import java.io.PrintStream
import java.lang.Long.toHexString
import java.lang.Float.intBitsToFloat
import java.lang.Double.longBitsToDouble
import scala.annotation.tailrec

import PickleFormat._

object ShowPickled extends Names {
  final case class PickleBufferEntry(num: Int, startIndex: Int, tag: Int, bytes: Array[Byte]) {
    override def toString = s"$num,$startIndex: ${tag2string(tag)}"
  }

  def readNat(data: Array[Byte]): Int = {
    var idx = 0
    var res = 0L
    var b   = 0L
    do {
      b    = data(idx).toLong
      idx += 1
      res  = (res << 7) + (b & 0x7f)
    } while ((b & 0x80) != 0L)
    res.toInt
  }

  final case class PickleBufferEntryList(entries: IndexedSeq[PickleBufferEntry]) {
    def nameAt(idx: Int) = {
      val entry = entries(idx)
      def utf8Str(bytes: Array[Byte]) = new String(bytes, "UTF-8")
      def readStr()    = utf8Str(entry.bytes)
      def readStrRef() = utf8Str(entries(readNat(entry.bytes)).bytes)
      entry.tag match {
        case TERMname       => readStr()
        case TYPEname       => readStr()
        case   TYPEsym      => readStrRef()
        case  ALIASsym      => readStrRef()
        case  CLASSsym      => readStrRef()
        case MODULEsym      => readStrRef()
        case    VALsym      => readStrRef()
        case         EXTref => readStrRef()
        case EXTMODCLASSref => readStrRef()
        case _              => "?"
      }
    }
  }

  def printPickle(buf: PickleBuffer, out: PrintStream): Unit = {
    out.println(s"Version ${buf.readNat()}.${buf.readNat()}")

    val index   = buf.createIndex
    val entries = PickleBufferEntryList(buf.toIndexedSeq.zipWithIndex.map({ case ((tag, data), num) =>
      PickleBufferEntry(num, index(num), tag, data)
    }))
    buf.readIndex = 0

    def p(s: String)            = out.print(s)
    def nameAt(idx: Int)        = s"$idx(${entries.nameAt(idx)})"
    def printNat()              = p(s" ${buf.readNat()}")
    def printNameRef()          = p(s" ${nameAt(buf.readNat())}")
    def printSymbolRef()        = printNat()
    def printTypeRef()          = printNat()
    def printConstantRef()      = printNat()
    def printAnnotInfoRef()     = printNat()
    def printConstAnnotArgRef() = printNat()
    def printAnnotArgRef()      = printNat()

    def printSymInfo(end: Int): Unit = {
      printNameRef()
      printSymbolRef() // owner
      val pflags = buf.readLongNat()
      val rflags = Flags.pickledToRawFlags(pflags)
      val (flagString, info) = buf.readNat() match {
        case pw if buf.readIndex != end => (Flags.flagsToString(rflags, nameAt(pw)), buf.readNat())
        case info                       => (Flags.flagsToString(rflags), info)
      }
      p(s" ${toHexString(pflags)}[$flagString] $info")
    }

    def printEntry(i: Int): Unit = {
      buf.readIndex = index(i)
      p(s"$i,${buf.readIndex}: ")

      val tag = buf.readByte()
      p(tag2string(tag))

      val len = buf.readNat()
      val end = len + buf.readIndex
      p(s" $len:")

      def all[T](body: => T) = buf.until(end, () => body)
      def printTypes()       = all(printTypeRef())
      def printSymbols()     = all(printSymbolRef())

      @tailrec def printTag(tag: Int): Unit = tag match {
        case  TERMname         => p(s" ${newTermName(buf.bytes, buf.readIndex, len)}"); buf.readIndex = end
        case  TYPEname         => p(s" ${newTypeName(buf.bytes, buf.readIndex, len)}"); buf.readIndex = end

        case   NONEsym         =>
        case   TYPEsym         => printSymInfo(end)
        case  ALIASsym         => printSymInfo(end)
        case  CLASSsym         => printSymInfo(end); if (buf.readIndex < end) printTypeRef()
        case MODULEsym         => printSymInfo(end)
        case    VALsym         => printSymInfo(end)

        case EXTref            => printNameRef()   ; if (buf.readIndex < end) printSymbolRef()
        case EXTMODCLASSref    => printNameRef()   ; if (buf.readIndex < end) printSymbolRef()

        case             NOtpe =>
        case       NOPREFIXtpe =>
        case           THIStpe => printSymbolRef()
        case         SINGLEtpe => printTypeRef()   ; printSymbolRef()
        case       CONSTANTtpe => printTypeRef()   ; printConstantRef()
        case        TYPEREFtpe => printTypeRef()   ; printSymbolRef()   ; printTypes()
        case     TYPEBOUNDStpe => printTypeRef()   ; printTypeRef()
        case        REFINEDtpe => printSymbolRef() ; printTypes()
        case      CLASSINFOtpe => printSymbolRef() ; printTypes()
        case         METHODtpe => printTypeRef()   ; printTypes()
        case           POLYtpe => printTypeRef()   ; printSymbols()
        case IMPLICITMETHODtpe => printTypeRef()   ; printTypes()
        case          SUPERtpe => printTypeRef()   ; printTypeRef()

        case LITERALunit       =>
        case LITERALboolean    => p(if (buf.readLong(len) == 0L) " false" else " true")
        case LITERALbyte       => p(" " + buf.readLong(len).toByte)
        case LITERALshort      => p(" " + buf.readLong(len).toShort)
        case LITERALchar       => p(" " + buf.readLong(len).toChar)
        case LITERALint        => p(" " + buf.readLong(len).toInt)
        case LITERALlong       => p(" " + buf.readLong(len))
        case LITERALfloat      => p(" " + intBitsToFloat(buf.readLong(len).toInt))
        case LITERALdouble     => p(" " + longBitsToDouble(buf.readLong(len)))
        case LITERALstring     => printNameRef()
        case LITERALnull       => p(" <null>")
        case LITERALclass      => printTypeRef()
        case LITERALenum       => printSymbolRef()
        case LITERALsymbol     => printNameRef()

        case SYMANNOT          => printSymbolRef() ; printTag(ANNOTINFO)
        case CHILDREN          => printSymbolRef() ; printSymbols()
        case     ANNOTATEDtpe  => printTypeRef()   ; all(printAnnotInfoRef())
        case ANNOTINFO         => printTypeRef()   ; all(printAnnotArgRef())
        case ANNOTARGARRAY     => all(printConstAnnotArgRef())
        case DEBRUIJNINDEXtpe  => printNat()       ; printNat()
        case   EXISTENTIALtpe  => printTypeRef()   ; printSymbols()

        case TREE              => // skipped
        case MODIFIERS         => // skipped

        case _ => throw new RuntimeException(s"malformed Scala signature at ${buf.readIndex}; unknown tree type ($tag)")
      }
      printTag(tag)

      out.println()
      if (buf.readIndex != end) {
        val bytes  = buf.bytes.slice(index(i), end.max(buf.readIndex)).mkString(", ")
        out.println(s"BAD ENTRY END: computed = $end, actual = ${buf.readIndex}, bytes = $bytes")
      }
    }

    for (i <- index.indices)
      printEntry(i)
  }

  def tag2string(tag: Int): String = tag match {
    case TERMname          => "TERMname"
    case TYPEname          => "TYPEname"

    case   NONEsym         =>   "NONEsym"
    case   TYPEsym         =>   "TYPEsym"
    case  ALIASsym         =>  "ALIASsym"
    case  CLASSsym         =>  "CLASSsym"
    case MODULEsym         => "MODULEsym"
    case    VALsym         =>    "VALsym"

    case         EXTref    =>         "EXTref"
    case EXTMODCLASSref    => "EXTMODCLASSref"

    case             NOtpe =>             "NOtpe"
    case       NOPREFIXtpe =>       "NOPREFIXtpe"
    case           THIStpe =>           "THIStpe"
    case         SINGLEtpe =>         "SINGLEtpe"
    case       CONSTANTtpe =>       "CONSTANTtpe"
    case        TYPEREFtpe =>        "TYPEREFtpe"
    case     TYPEBOUNDStpe =>     "TYPEBOUNDStpe"
    case        REFINEDtpe =>        "REFINEDtpe"
    case      CLASSINFOtpe =>      "CLASSINFOtpe"
    case         METHODtpe =>         "METHODtpe"
    case           POLYtpe =>           "POLYtpe"
    case IMPLICITMETHODtpe => "IMPLICITMETHODtpe" // no longer used
    case          SUPERtpe =>          "SUPERtpe"

    case LITERALunit       => "LITERALunit"
    case LITERALboolean    => "LITERALboolean"
    case LITERALbyte       => "LITERALbyte"
    case LITERALshort      => "LITERALshort"
    case LITERALchar       => "LITERALchar"
    case LITERALint        => "LITERALint"
    case LITERALlong       => "LITERALlong"
    case LITERALfloat      => "LITERALfloat"
    case LITERALdouble     => "LITERALdouble"
    case LITERALstring     => "LITERALstring"
    case LITERALnull       => "LITERALnull"
    case LITERALclass      => "LITERALclass"
    case LITERALenum       => "LITERALenum"
    case LITERALsymbol     => "LITERALsymbol"

    case SYMANNOT          => "SYMANNOT"
    case CHILDREN          => "CHILDREN"
    case      ANNOTATEDtpe =>      "ANNOTATEDtpe"
    case ANNOTINFO         => "ANNOTINFO"
    case ANNOTARGARRAY     => "ANNOTARGARRAY"
    case    EXISTENTIALtpe =>    "EXISTENTIALtpe"

    case TREE              => "TREE"
    case MODIFIERS         => "MODIFIERS"
    case _                 => s"***BAD TAG***($tag)"
  }
}
