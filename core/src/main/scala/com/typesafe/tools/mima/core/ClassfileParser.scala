package com.typesafe.tools.mima.core

import java.io.IOException

import ClassfileConstants._

final class ClassfileParser private (in: BufferReader, pool: ConstantPool) {
  import ClassfileParser._

  private def parseClass(clazz: ClassInfo): Unit = {
    val flags = in.nextChar
    clazz._flags = flags
    in.skip(2) // external name index

    clazz._superClass = parseSuperClass(clazz, flags)
    clazz._interfaces = parseInterfaces()
    clazz._fields     = parseMembers[FieldInfo](clazz)
    clazz._methods    = parseMembers[MethodInfo](clazz)
    parseClassAttributes(clazz)
  }

  private def parseSuperClass(clazz: ClassInfo, flags: Int): ClassInfo = {
    if (isAnnotation(flags)) {
      in.skip(2)
      clazz.owner.definitions.AnnotationClass
    } else pool.getSuperClass(in.nextChar)
  }

  private def parseInterfaces(): List[ClassInfo] = {
    List.fill(in.nextChar)(pool.getSuperClass(in.nextChar))
  }

  private def parseMembers[A <: MemberInfo : MkMember](clazz: ClassInfo): Members[A] = {
    val members = for {
      _ <- 0.until(in.nextChar).iterator
      flags = in.nextChar
      _ = if (isPrivate(flags)) { in.skip(4); parseAttributes(_ => ()) }
      if !isPrivate(flags)
    } yield parseMember[A](clazz, flags)
    new Members(members.toList)
  }

  private def parseMember[A <: MemberInfo : MkMember](clazz: ClassInfo, flags: Int): A = {
    val name       = pool.getName(in.nextChar)
    val descriptor = pool.getExternalName(in.nextChar)
    val member     = implicitly[MkMember[A]].make(clazz, name, flags, descriptor)
    parseMemberAttributes(member)
    member
  }

  private def parseClassAttributes(clazz: ClassInfo) = {
    var isScala, isScalaRaw = false
    var runtimeAnnotStart = -1
    parseAttributes {
      case RuntimeAnnotationATTR => runtimeAnnotStart = in.bp
      case ScalaSignatureATTR    => isScala    = true
      case ScalaATTR             => isScalaRaw = true
      case EnclosingMethodATTR   => clazz._isLocalClass = true
      case InnerClassesATTR      => clazz._innerClasses = parseInnerClasses(clazz)
      case _                     =>
    }
    if (isScala) {
      val end = in.bp
      in.bp = runtimeAnnotStart
      parsePickle(clazz)
      in.bp = end
    }
  }

  private def parseMemberAttributes(member: MemberInfo) = {
    parseAttributes {
      case DeprecatedATTR => member.isDeprecated = true
      case SignatureATTR  => member.signature = Signature(pool.getName(in.nextChar))
      case _              =>
    }
  }

  private def parseAttributes(processAttr: String => Unit) = {
    for (_ <- 0 until in.nextChar) {
      val attrIdx = in.nextChar
      val attrLen = in.nextInt
      val attrEnd = in.bp + attrLen
      processAttr(pool.getName(attrIdx))
      in.bp = attrEnd
    }
  }

  private def parseInnerClasses(c: ClassInfo) = for {
    _ <- 0 until in.nextChar
    (innerIndex, outerIndex, innerNameIndex) = (in.nextChar, in.nextChar, in.nextChar)
    _ = in.skip(2) // inner class flags
    if innerIndex != 0 && outerIndex != 0 && innerNameIndex != 0
    if pool.getClassName(outerIndex) == c.bytecodeName
  } yield {
    val className = pool.getClassName(innerIndex)
    if (className == c.bytecodeName) c._isTopLevel = false // an inner class lists itself in InnerClasses
    className
  }

  private def parsePickle(clazz: ClassInfo) = {
    def parseScalaSigBytes()     = {
      assert(in.nextByte == STRING_TAG)
      pool.getBytes(in.nextChar, in.bp)
    }

    def parseScalaLongSigBytes() = {
      assert(in.nextByte == ARRAY_TAG)
      val entries = for (_ <- 0 until in.nextChar) yield {
        val tag = in.nextByte; assert(tag == STRING_TAG, tag)
        in.nextByte.toInt
      }
      pool.getBytes(entries.toList, in.bp)
    }

    def checkScalaSigAnnotArg() = {
      val numArgs = in.nextChar               ; assert(numArgs == 1,    s"ScalaSignature has $numArgs arguments")
      val name    = pool.getName(in.nextChar) ; assert(name == "bytes", s"ScalaSignature argument has name $name")
    }

    def skipAnnotArg(): Unit = in.nextByte match {
      case   BOOL_TAG |   BYTE_TAG => in.skip(2)
      case   CHAR_TAG |  SHORT_TAG => in.skip(2)
      case    INT_TAG |   LONG_TAG => in.skip(2)
      case  FLOAT_TAG | DOUBLE_TAG => in.skip(2)
      case STRING_TAG |  CLASS_TAG => in.skip(2)
      case ENUM_TAG                => in.skip(4)
      case ARRAY_TAG               => for (_ <- 0 until in.nextChar) skipAnnotArg()
      case ANNOTATION_TAG          => in.skip(2); /* type */ skipAnnotArgs()
    }

    def skipAnnotArgs() = for (_ <- 0 until in.nextChar) { in.skip(2); skipAnnotArg() }

    var i = 0
    var bytes: Array[Byte] = null
    while (i < in.nextChar && bytes == null) {
      pool.getExternalName(in.nextChar) match {
        case ScalaSignatureAnnot     => checkScalaSigAnnotArg(); bytes = parseScalaSigBytes()
        case ScalaLongSignatureAnnot => checkScalaSigAnnotArg(); bytes = parseScalaLongSigBytes()
        case _                       => skipAnnotArgs()
  }
      i += 1
    }
    if (bytes != null) {
      val pb = new PickleBuffer(bytes, 0, bytes.length)
      MimaUnpickler.unpickleClass(pb, clazz, in.path)
    }
  }

  private final val ScalaSignatureAnnot     = "Lscala.reflect.ScalaSignature;"
  private final val ScalaLongSignatureAnnot = "Lscala.reflect.ScalaLongSignature;"

  private final val DeprecatedATTR        = "Deprecated"
  private final val EnclosingMethodATTR   = "EnclosingMethod"
  private final val InnerClassesATTR      = "InnerClasses"
  private final val RuntimeAnnotationATTR = "RuntimeVisibleAnnotations"
  private final val ScalaATTR             = "Scala"
  private final val ScalaSignatureATTR    = "ScalaSig"
  private final val SignatureATTR         = "Signature"
}

object ClassfileParser {
  private[core] def parseInPlace(clazz: ClassInfo, file: AbsFile): Unit = {
    val in = new BufferReader(file.toByteArray, file.toString)
    parseHeader(in, file.toString)
    val pool = ConstantPool.parseNew(clazz.owner.definitions, in)
    val parser = new ClassfileParser(in, pool)
    parser.parseClass(clazz)
  }

  def isPublic(flags: Int)     = 0 != (flags & JAVA_ACC_PUBLIC)
  def isPrivate(flags: Int)    = 0 != (flags & JAVA_ACC_PRIVATE)
  def isProtected(flags: Int)  = 0 != (flags & JAVA_ACC_PROTECTED)
  def isStatic(flags: Int)     = 0 != (flags & JAVA_ACC_STATIC)
  def isFinal(flags: Int)      = 0 != (flags & JAVA_ACC_FINAL)
  def isInterface(flags: Int)  = 0 != (flags & JAVA_ACC_INTERFACE)
  def isDeferred(flags: Int)   = 0 != (flags & JAVA_ACC_ABSTRACT)
  def isSynthetic(flags: Int)  = 0 != (flags & JAVA_ACC_SYNTHETIC)
  def isAnnotation(flags: Int) = 0 != (flags & JAVA_ACC_ANNOTATION)

  private def parseHeader(in: BufferReader, file: String) = {
    val magic = in.nextInt
    if (magic != JAVA_MAGIC)
      throw new IOException(
        s"class file '$file' has wrong magic number 0x${magic.toHexString}, " +
            s"should be 0x${JAVA_MAGIC.toHexString}")
    in.skip(2) // minorVersion
    in.skip(2) // majorVersion
  }

  private trait MkMember[A] {
    def make(owner: ClassInfo, bytecodeName: String, flags: Int, descriptor: String): A
  }

  private implicit def mkFieldInfo: MkMember[FieldInfo]   = new FieldInfo(_, _, _, _)
  private implicit def mkMethodInfo: MkMember[MethodInfo] = new MethodInfo(_, _, _, _)
}
