package com.qdx.suffixtree

class ActivePoint[T](a_node: Node[T], a_edge: Option[T] , a_length: Int) {
  var node = a_node
  var edge_head = a_edge
  var length = a_length

  // whenever length is greater than 0, edge_head should not be None
  if(edge_head.isEmpty) assert(length == 0)

  def get_edge(): Option[Edge[T]] = {
    if(edge_head.isDefined) Some(node.edges(edge_head.get))
    else None
  }
}
