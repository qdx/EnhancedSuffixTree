package com.qdx.debugging

object Logger {
  val ERROR = 0
  val WARN = 1
  val DEBUG = 2
  val INFO = 3
}

trait Logger {
  var log_level = Logger.DEBUG

  def debug(s: String): Unit  =  if(log_level >= Logger.DEBUG) log(s)
  def info(s: String): Unit  =  if(log_level >= Logger.INFO) log(s)
  def warn(s: String): Unit  =  if(log_level >= Logger.WARN) log(s)
  def error(s: String): Unit  =  if(log_level >= Logger.ERROR) log(s)

  private def log(s: String): Unit = {
    val log_head = log_level match {
      case Logger.INFO => "[INFO]: "
      case Logger.WARN => "[WARN]: "
      case Logger.ERROR => "[ERROR]: "
      case Logger.DEBUG => "[DEBUG]: "
    }
    println(log_head + s)
  }

}
