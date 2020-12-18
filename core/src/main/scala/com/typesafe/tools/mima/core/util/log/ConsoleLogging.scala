package com.typesafe.tools.mima.core.util.log

private[mima] object ConsoleLogging extends Logging {
  private final val debug  = true

  def verbose(msg: String) = debug(msg)
  def debug(msg: String)   = if (debug) Console.out.println(msg)
  def warn(msg: String)    = Console.out.println(msg)
  def error(msg: String)   = Console.err.println(msg)
}
