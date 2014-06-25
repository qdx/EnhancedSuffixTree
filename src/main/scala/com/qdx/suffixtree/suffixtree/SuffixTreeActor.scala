package com.qdx.suffixtree.suffixtree

import akka.actor.Actor
import com.qdx.suffixtree.regex.Pattern
import com.qdx.suffixtree.demo.Tick
import com.qdx.suffixtree.demo.MainEntry
import java.io.{File, FileWriter}
import scala.collection.{mutable => m}
import com.github.jabbalaci.graphviz.GraphViz
import java.awt.Desktop

class SuffixTreeActor extends Actor {
  var suffix_tree = new SuffixTree[Char]
//  suffix_tree.window_size = 10
//  suffix_tree.slide_size = 1
  var pattern_cache = new Pattern("")
  var result_buffer = new m.HashSet[(BigInt, Int)]()

  def receive = {
    case input: String =>
      if(input.startsWith("#clear:")){
        val sizes = input.split(":")(1)
        val window_size = sizes.split(",")(0).toInt
        val slide_size = sizes.split(",")(1).toInt
        suffix_tree = new SuffixTree[Char]
        suffix_tree.window_size = window_size
        suffix_tree.slide_size = slide_size
        pattern_cache = new Pattern("")
        result_buffer = new m.HashSet[(BigInt, Int)]()
      }
      else input_and_write_file(input)
    case c: Char =>
      input_and_write_file(c.toString)
    case pattern: Pattern =>
      pattern_cache = pattern
      query_output()
    case t: Tick =>
      query_output()
    case _ =>
  }

  private def input_and_write_file(str: String): Unit = {
    val change_flag = str.map(c => suffix_tree.insert_wrapper(c)).exists(t => t)
    if (change_flag) {
      query_output()
      write_current_input_to_file()
      open_tree_img()
    }
  }

  private def query_output(): Unit = {
    val result = pattern_cache.search_pattern(suffix_tree)
    val result_set = new m.HashSet[(BigInt, Int)]()
    result_set ++= result
    if (!result_set.equals(result_buffer)) {
      result_buffer = result_set
      val result_str = result.sorted.mkString(", ")
      val fw = new FileWriter(MainEntry.demo_file_path + "query_result.txt", true)
      try {
        fw.write(result_str + "\n")
      }
      finally {
        fw.close()
      }
    }
  }

  private def write_current_input_to_file(): Unit = {
    val fw_input = new FileWriter(MainEntry.demo_file_path + "input.txt", true)
    try {
      fw_input.write(suffix_tree.sequence.mkString + "\n" + (0 until 80).map(_ => "-").mkString + "\n")
    }
    finally {
      fw_input.close()
    }
  }

  private def open_tree_img(): Unit = {
    val gv = new GraphViz()
    gv.increaseDpi()
    // 106 dpi
    val gtype = "gif"
    val rtype = "dot"
    val img_path = MainEntry.demo_file_path + "out." + gtype
    val out = new File(img_path) // Windows
    gv.writeGraphToFile(gv.getGraph(suffix_tree.show(), gtype, rtype), out)
    val f = new File(img_path)
    val dt = Desktop.getDesktop
    dt.open(f)
  }
}
