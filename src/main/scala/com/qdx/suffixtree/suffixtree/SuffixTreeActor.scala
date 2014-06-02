package com.qdx.suffixtree.suffixtree

import akka.actor.Actor
import com.qdx.suffixtree.regex.Pattern
import com.qdx.suffixtree.demo.Tick
import java.io.FileWriter
import scala.collection.{mutable => m}

class SuffixTreeActor extends Actor {
  val suffix_tree = new SuffixTree[Char]
  var pattern_cache = new Pattern("")
  var result_buffer = new m.HashSet[(BigInt, Int)]()

  def receive = {
    case input: String =>
      suffix_tree.batch_input(input)
      query_output()
    case c: Char =>
      suffix_tree.insert(c)
      query_output()
    case pattern: Pattern =>
      pattern_cache = pattern
      query_output()
    case t: Tick =>
      query_output()
    case _ =>
  }

  def query_output(): Unit = {
    val result = pattern_cache.search_pattern(suffix_tree)
    val result_set = new m.HashSet[(BigInt, Int)]()
    result_set ++= result
    if (!result_set.equals(result_buffer)) {
      result_buffer = result_set
      val result_str = result.sorted.mkString(", ")
      val fw = new FileWriter("query_result.txt", true)
      try {
        fw.write(result_str + "\n")
      }
      finally {
        fw.close()
      }
    }
  }
}
