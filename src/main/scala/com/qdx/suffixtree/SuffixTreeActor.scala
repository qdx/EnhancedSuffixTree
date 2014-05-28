package com.qdx.suffixtree

import akka.actor.Actor
import com.qdx.regex.Pattern
import com.qdx.demo.Tick
import java.io.FileWriter

class SuffixTreeActor extends Actor {
  val suffix_tree = new SuffixTree[Char]
  var pattern_cache = new Pattern("")

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
    val result = pattern_cache.search_pattern(suffix_tree).sorted.mkString(" ")
    val fw = new FileWriter("query_result.txt", true)
    try {
      fw.write(result + "\n")
    }
    finally {
      fw.close()
    }
  }
}
