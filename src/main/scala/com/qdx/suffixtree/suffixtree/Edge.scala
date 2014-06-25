package com.qdx.suffixtree.suffixtree

import scala.collection.mutable.ArrayBuffer

case class Label(start: BigInt, end: BigInt)

class Edge[T](start: BigInt, end: BigInt, from_node: Node[T], to_node: Node[T]) {
  // includes end
  var label = new Label(start, end)
  var to = to_node
  var from = from_node

  // ALTERNATE: refactor this method to eliminate the parameter
  def length(seq_length: Int, window_head: BigInt): Int =
    if (label.end == SuffixTree.SEQ_END)
      (seq_length - (label.start - window_head)).toInt
    else
      (label.end - label.start + 1).toInt


  def get_label_seq(seq: ArrayBuffer[T], window_head: BigInt): ArrayBuffer[T] = {
    val end = if (label.end == SuffixTree.SEQ_END) seq.length - 1 else (label.end - window_head).toInt
    val label_seq = seq.slice((label.start - window_head).toInt, end + 1)
    label_seq
  }

}
