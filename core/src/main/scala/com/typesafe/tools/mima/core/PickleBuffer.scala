package com.typesafe.tools.mima.core

/** Variable length byte arrays, with methods for basic unpickling.
 *
 *  @param data The initial buffer
 *  @param from The first index where defined data are found
 *  @param to   The first index where new data can be written
 */
class PickleBuffer(data: Array[Byte], from: Int, to: Int) {
  var bytes     = data
  var readIndex = from

  def readByte(): Int = { val x = bytes(readIndex).toInt; readIndex += 1; x }
  def lastByte(): Int = bytes(readIndex - 1).toInt

  /** Read a natural number in big endian format, base 128. All but the last digits have bit 0x80 set. */
  def readNat(): Int = readLongNat().toInt

  def readLongNat(): Long = {
    var b = 0L
    var x = 0L
    do {
      b = readByte().toLong
      x = (x << 7) + (b & 0x7f)
    } while ((b & 0x80) != 0L)
    x
  }

  /** Read a long number in signed big endian format, base 256. */
  def readLong(len: Int): Long = {
    var x = 0L
    var i = 0
    while (i < len) {
      x = (x << 8) + (readByte() & 0xff)
      i += 1
    }
    val leading = 64 - (len << 3)
    x << leading >> leading
  }

  /** Perform operation `op` until the condition `readIndex == end` is satisfied. Concatenate results into a list. */
  def until[T](end: Int, op: () => T): List[T] = if (readIndex == end) List.empty[T] else op() :: until(end, op)

  def createIndex: Array[Int] = {
    runAtReadIndex(0) {
      readNat(); readNat() // discard version
      (0 until readNat()).map { _ =>
        val start = readIndex
        readByte()                        // skip type_Nat
        readIndex = readNat() + readIndex // read length_Nat, jump to next entry
        start
      }.toArray
    }
  }

  def slice(start: Int, end: Int) = data.slice(start, end)

  /** Returns the buffer as a sequence of (Int, Array[Byte]) representing
   *  (tag, data) of the individual entries.  Saves and restores buffer state.
   */
  def toIndexedSeq: IndexedSeq[(Int, Array[Byte])] = {
    runAtReadIndex(0) {
      readNat(); readNat() // discard version
      (0 until readNat()).map { _ =>
        val tag   = readByte()
        val len   = readNat()
        val start = readIndex
        readIndex += len
        tag -> data.slice(start, readIndex)
      }
    }
  }

  final def runAtReadIndex[T](i: Int)(body: => T): T = {
    val saved = readIndex
    try {
      readIndex = i
      body
    } finally readIndex = saved
  }
}
