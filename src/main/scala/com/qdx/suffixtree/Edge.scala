package com.qdx.suffixtree

case class Label(start: Int, end: Int)

class Edge[T](start: Int, end: Int, to_node: Node[T]) {
  // includes end
  var label = new Label(start, end)
  var to = to_node

  def length(seq_length: Int): Int = {
    if (label.end == -1) seq_length - label.start else label.end - label.start + 1
  }

}
