package com.suffixtree

// FIXME: constraint that I did not enforce:
// whenever length is greater than 0, edge_head should not be None
class ActivePoint[T](a_node: Node[T], a_edge: Option[T] , a_length: Int) {
  var node = a_node
  var edge_head = a_edge
  var length = a_length
}
