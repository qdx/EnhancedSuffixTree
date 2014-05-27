package com.qdx.suffixtree

import scala.collection.mutable.ArrayBuffer

case class Label(start: Int, end: Int)

class Edge[T](start: Int, end: Int, to_node: Node[T]) {
  // includes end
  var label = new Label(start, end)
  var to = to_node

  // ALTERNATE: refactor this method to eliminate the parameter
  def length(seq_length: Int): Int = {
    if (label.end == -1) seq_length - label.start else label.end - label.start + 1
  }

  def get_label_seq(seq: ArrayBuffer[T]): ArrayBuffer[T] = {
    val end = if (label.end == -1) seq.length - 1 else label.end
    val label_seq = seq.slice(label.start, end + 1)
    label_seq
  }

}
