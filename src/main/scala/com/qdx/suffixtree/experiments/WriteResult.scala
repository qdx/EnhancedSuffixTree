package com.qdx.suffixtree.experiments

import java.io.FileWriter

object WriteResult {

  def write_to(filename:String, result: String, title: String = ""): Unit = {
    val fw = new FileWriter(filename, true)
    try {
      fw.write(title + "\n")
      fw.write(result  + "\n")
    }
    finally {
      fw.close()
    }
  }
}
