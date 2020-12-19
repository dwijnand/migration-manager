package com.typesafe.tools.mima.core

import scala.annotation.tailrec
import scala.collection.mutable
import PickleFormat._

import scala.collection.mutable.ListBuffer

object MimaUnpickler {
  def unpickleClass(buf: PickleBuffer, clazz: ClassInfo, path: String) = {
    //val doPrint = path.contains("v1") && !path.contains("Lib")
    //if (doPrint && path.toLowerCase.contains("foo")) {
    //  println(s"unpickling $path")
    //  ShowPickled.printPickle(buf, Console.out)
    //  buf.readIndex = 0
    //}

    buf.readNat(); buf.readNat() // major, minor version

    val origClass = clazz
    val index     = buf.createIndex

    def nameAt(num: Int) = {
      buf.runAtReadIndex(index(num)) {
        val tag   = buf.readByte()
        val len   = buf.readNat()
        val bytes = buf.slice(buf.readIndex, buf.readIndex + len)
        def readStrRef() = {
          val stringBytes = buf.runAtReadIndex(index(readNat(bytes))) {
            buf.readByte() // tag
            val len = buf.readNat()
            buf.slice(buf.readIndex, buf.readIndex + len)
          }
          utf8Str(stringBytes)
        }
        tag match {
          case TERMname       => utf8Str(bytes)
          case TYPEname       => utf8Str(bytes)
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

    def getNextNum() = index.indices.find(index(_) >= buf.readIndex).getOrElse(index.length)
    def    readEnd() = buf.readNat() + buf.readIndex
    def    nameRef() = nameAt(buf.readNat())
    def   ownerRef() = buf.readNat()

    final case class SymbolInfo(tag: Int, name: String, owner: Int, isScopedPrivate: Boolean) {
      override def toString = {
        val tag_s = tag match {
          case  CLASSsym =>  "CLASSsym"
          case MODULEsym => "MODULEsym"
          case _         => s"$tag"
        }
        s"SymbolInfo($tag_s, $name, owner=$owner, isScopedPrivate=$isScopedPrivate)"
      }
    }

    // SymbolInfo = name_Ref owner_Ref flags_LongNat [privateWithin_Ref] info_Ref
    def readSymbolInfo(): SymbolInfo = {
      val tag   = buf.lastByte()
      val end   =  readEnd()
      val name  =  nameRef()
      val owner = ownerRef()
      buf.readLongNat() // flags
      buf.readNat()     // privateWithin or symbol info (compare to end)
      val isScopedPrivate = buf.readIndex != end
      buf.readIndex = end
      SymbolInfo(tag, name, owner, isScopedPrivate)
    }

    val classes = new mutable.HashMap[Int, ClassInfo]()

    def readMethods(clazz: ClassInfo) = {
      val methods = new ListBuffer[SymbolInfo]

      @tailrec def readBodyEntries(num: Int): Unit = {
        if (num >= index.length) return
        buf.readIndex = index(num)
        buf.readByte() match {
          case CLASSsym  => return
          case MODULEsym => return
          case VALsym    => methods += readSymbolInfo()
          case _         =>
        }
        readBodyEntries(num + 1)
      }
      readBodyEntries(getNextNum())

      // TODO support package private constructors
      methods.groupBy(_.name).filter(_._1 != "<init>").foreach { case (name, overloads) =>
        val methods = clazz.methods.get(name).toList
        if (methods.nonEmpty && overloads.exists(_.isScopedPrivate)) {
          assert(overloads.size == methods.size, s"size mismatch; methods=$methods overloads=$overloads")
          methods.zip(overloads).foreach { case (method, symbolInfo) =>
            method.scopedPrivate = symbolInfo.isScopedPrivate
          }
        }
      }
    }

    sealed trait MCMC
    case object M  extends MCMC
    case object C  extends MCMC
    case object MC extends MCMC

    @tailrec def read(num: Int, mcmc: MCMC): Unit = {
      val symbolInfo = readSymbolInfo()

      val clazz = num match {
        case 1 => origClass.moduleClass
        case _ => classes.get(symbolInfo.owner) match {
          case None        => origClass
          case Some(owner) =>
            val nme1 = owner.bytecodeName
            val nme2 = symbolInfo.name
            val conc = if (nme1.endsWith("$")) "" else "$"
            val suff = if (mcmc == C) "" else "$"
            val cls  = origClass.owner.classes(nme1 + conc + nme2 + suff)
            cls
        }
      }

      if (symbolInfo.isScopedPrivate)
        clazz.module._scopedPrivate = true

      classes(num) = clazz

      readMethods(if (mcmc == MC) clazz.moduleClass else clazz)

      buf.lastByte() match {
        case  CLASSsym if mcmc == M => read(getNextNum() - 1, MC)
        case  CLASSsym              => read(getNextNum() - 1, C)
        case MODULEsym              => read(getNextNum() - 1, M)
        case _                      =>
      }
    }

    buf.readIndex = index(0)
    try {
      buf.readByte() match {
        case  CLASSsym => read(0, C)
        case MODULEsym => read(0, M)
        case _         =>
      }
    } catch {
      case scala.util.control.NonFatal(e) =>
        println(e)
        e.getStackTrace.take(10).foreach(x => println(s"        at $x"))
        throw e
    }
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

  def utf8Str(bytes: Array[Byte]) = new String(bytes, "UTF-8")
}
