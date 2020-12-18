package com.typesafe.tools.mima.core

import scala.annotation.tailrec

import PickleFormat._

object MimaUnpickler {
  def unpickleClass(buf: PickleBuffer, clazz: ClassInfo, path: String) = {
    //if (path.toLowerCase.contains("foo") && path.contains("v1")) {
    //  println(s"unpickling $path")
    //  ShowPickled.printPickle(buf, Console.out)
    //  buf.readIndex = 0
    //}

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
    def readMeth(): Meth = {
      val end  = readEnd()
      val name = nameRef()
      buf.readNat()     // owner
      buf.readLongNat() // flags
      buf.readNat()     // privateWithin or symbol info (compare to end)
      val isScopedPrivate = buf.readIndex != end
      buf.readIndex = end
      (name, isScopedPrivate)
    }

    @tailrec def read(clazz: ClassInfo): Unit = {
      symbolInfo(clazz)

      val methods = new scala.collection.mutable.ListBuffer[Meth]
      @tailrec def readBodyEntries(num: Int): Unit = {
        if (num >= index.length) return
        buf.readIndex = index(num)
        buf.readByte() match {
          case  CLASSsym => return
          case MODULEsym => return
          case    VALsym => methods += readMeth()
          case _         =>
        }
        readBodyEntries(num + 1)
      }
      index.indices.find(index(_) >= buf.readIndex).foreach(readBodyEntries)

      methods.groupBy(_._1).foreach { case (name, overloads) =>
        val methods = clazz._methods.get(name).toList
        if (methods.nonEmpty && overloads.exists(_._2)) {
          assert(overloads.size == methods.size, s"size mismatch; methods=$methods overloads=$overloads")
          methods.zip(overloads).foreach { case (method, (_, isScopedPrivate)) =>
            method.scopedPrivate = isScopedPrivate
          }
        }
      }

      // TODO next:
      // need to find the right ClassInfo instance
      // consider: CLASSsym then CLASSsym, like `class x { class y }`
      // consider: asserting name in symbolInfo?
      // consider: keeping way to lookup entry num -> ClassInfo
      //   so that:
      //     0, 3: MODULEsym 5: 13(x) 14 400[<module>] 19
      //     1,10:  CLASSsym 5: 21(x) 14 400[<module>] 22
      //     3,24: MODULEsym 5: 31(y)  1 400[<module>] 32
      //     4,31:  CLASSsym 5: 34(y)  1 400[<module>] 35
      //     6,45: MODULEsym 5: 37(z)  4 400[<module>] 38
      //     7,52:  CLASSsym 5: 40(z)  4 400[<module>] 41
      //       3/4 can find its owner (1)'s ClassInfo?
      //   and 6/7 can find its owner (4)'s ClassInfo?
      //   perhaps just for its name so we can mangle some more and look up the class
      buf.lastByte() match {
        case  CLASSsym => read(clazz.moduleClass)
        case MODULEsym => read(clazz)
        case _         =>
      }
    }

    buf.readIndex = index(0)
    try {
      buf.readByte() match {
        case  CLASSsym => read(clazz)
        case MODULEsym => read(clazz)
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
