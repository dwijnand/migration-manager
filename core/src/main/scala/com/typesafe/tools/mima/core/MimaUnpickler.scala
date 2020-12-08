package com.typesafe.tools.mima.core

import PickleFormat._

object MimaUnpickler {
  def unpickleClass(buf: PickleBuffer, clazz: ClassInfo, path: String) = {
    buf.readNat(); buf.readNat() // major, minor version

    val index = buf.createIndex

    def readEnd() = buf.readNat() + buf.readIndex
    def nameRef() = nameAt(buf.readNat())

    def nameAt(num: Int) = {
      val idx = index(num)
      buf.runAtReadIndex(idx) {
        val tag   = buf.readByte()
        val len   = buf.readNat()
        val bytes = buf.slice(buf.readIndex, buf.readIndex + len)
        def readStrRef() = {
          val bytes2 = buf.runAtReadIndex(index(readNat(bytes))) {
            buf.readByte() // tag
            val len = buf.readNat()
            buf.slice(buf.readIndex, buf.readIndex + len)
          }
          utf8Str(bytes2)
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

    def symbolInfo(): Unit = {
      val end = readEnd()
      buf.readNat()     // name
      buf.readNat()     // owner (symbol) ref
      buf.readLongNat() // flags
      buf.readNat()     // privateWithin or symbol info (compare to end)
      if (buf.readIndex != end) clazz._scopedPrivate = true
    }

    def valSym(): (String, Boolean) = {
      val end   = readEnd()
      val name  = nameRef()
      val owner = buf.readNat()
      buf.readLongNat() // flags
      buf.readNat()     // privateWithin or symbol info (compare to end)
      val isScopedPrivate = buf.readIndex != end && owner == 0
      buf.readIndex = end
      (name, isScopedPrivate)
    }

    def read(): Unit = {
      symbolInfo()

      val methods = index.indices.drop(1).toList.flatMap { i =>
        buf.readIndex = index(i)
        buf.readByte() match {
          case VALsym => List(valSym())
          case _      => Nil
        }
      }

      methods.groupBy(_._1).foreach { case (name, overloads) =>
        val methods = clazz.moduleClass._methods.get(name).toList
        if (methods.nonEmpty) { // fields are VALsym's with name "bar " (local)
          assert(overloads.size == methods.size, s"size mismatch; methods=$methods overloads=$overloads")
          methods.zip(overloads).foreach { case (method, (_, isScopedPrivate)) =>
            method.scopedPrivate = isScopedPrivate
          }
        }
      }
    }

    buf.readIndex = index(0)
    buf.readByte() match {
      case  CLASSsym => read()
      case MODULEsym => read()
      case _         =>
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
