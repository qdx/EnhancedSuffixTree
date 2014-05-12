package com.suffixtree

case class Label(start: Int, end: Int)

class Edge[T](start: Int, end: Int, to_node: Node[T]) {
  // includes end
  var label = new Label(start, end)
  var to = to_node

  def length(): Int = end - start + 1

}
