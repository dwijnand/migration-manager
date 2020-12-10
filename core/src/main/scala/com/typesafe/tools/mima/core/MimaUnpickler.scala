package com.typesafe.tools.mima.core

import scala.annotation.tailrec

import PickleFormat._

object MimaUnpickler {
  def unpickleClass(buf: PickleBuffer, clazz: ClassInfo, path: String) = {
    if (path.contains("Foo")) {
      println(s"unpickling $path")
      ShowPickled.printPickle(buf, Console.out)
      buf.readIndex = 0
    }

    buf.readNat(); buf.readNat() // major, minor version

    val index = buf.createIndex

    def readEnd() = buf.readNat() + buf.readIndex
    def nameRef() = nameAt(buf.readNat())

    def nameAt(num: Int) = {
      buf.runAtReadIndex(index(num)) {
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

    def symbolInfo(clazz: ClassInfo): Unit = {
      val end = readEnd()
      buf.readNat()     // name
      buf.readNat()     // owner (symbol) ref
      buf.readLongNat() // flags
      buf.readNat()     // privateWithin or symbol info (compare to end)
      if (buf.readIndex != end)
        clazz.module._scopedPrivate = true
    }

    type Meth = (String, Boolean)
    def valSym(): Meth = {
      val end   = readEnd()
      val name  = nameRef()
      buf.readNat()     // owner
      buf.readLongNat() // flags
      buf.readNat()     // privateWithin or symbol info (compare to end)
      val isScopedPrivate = buf.readIndex != end
      buf.readIndex = end
      (name, isScopedPrivate)
    }

    @tailrec def read(targetClass: ClassInfo): Unit = {
      symbolInfo(targetClass)

      val methods = new scala.collection.mutable.ListBuffer[Meth]
      @tailrec def readMethod(num: Int): Unit = {
        if (num >= index.length) return
        buf.readIndex = index(num)
        buf.readByte() match {
          case    VALsym => methods += valSym()
          case MODULEsym => return
          case  CLASSsym => return
          case _         =>
        }
        readMethod(num + 1)
      }
      readMethod(index.indices.drop(1).find(index(_) >= buf.readIndex).getOrElse(1))

      //println(s"clazz=$clazz")
      //println(s"found methods=$methods")
      //println(s"clazz methods=${clazz._methods.value}")

      methods.groupBy(_._1).foreach { case (name, overloads) =>
        val methods = targetClass._methods.get(name).toList
        if (methods.nonEmpty && overloads.exists(_._2)) { // fields are VALsym's with name "bar " (local)
          assert(overloads.size == methods.size, s"size mismatch; methods=$methods overloads=$overloads")
          methods.zip(overloads).foreach { case (method, (_, isScopedPrivate)) =>
            method.scopedPrivate = isScopedPrivate
          }
        }
      }

      if (buf.lastByte() == MODULEsym) {
        read(targetClass.moduleClass)
      } else if (buf.lastByte() == CLASSsym) {
        val startPoint = buf.readIndex
        val name       = nameRef()
        read(targetClass.moduleClass)
      }
    }

    buf.readIndex = index(0)
    try {
      buf.readByte() match {
        case  CLASSsym => read(clazz)
        case MODULEsym => read(clazz.moduleClass)
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
