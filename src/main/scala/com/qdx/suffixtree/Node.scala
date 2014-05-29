package com.qdx.suffixtree

import scala.collection.{mutable => m}
import scala.collection.mutable.ArrayBuffer

object Node {
  val ROOT = 0
  val LEAF_NODE = 1
  val INTERNAL_NODE = 2
}

class Node[T](node_type: Int = 2, search_index: BigInt = -1) {
  var from_edge = None: Option[Edge[T]]
  val edges = m.HashMap[T, Edge[T]]()
  var type_ = node_type
  var suffix_link = None : Option[Node[T]]
  var search_index_ = search_index

  // I don't know how to express the constraint:
  // Only a terminal node has a search index, a terminal
  // node can never have suffix link
  // root can never have suffix link
  if(type_ == Node.LEAF_NODE) assert(suffix_link.isEmpty)
  if(type_ == Node.ROOT) assert(suffix_link.isEmpty)

  def add_edge(seq: ArrayBuffer[T], edge: Edge[T]): Unit = {
    edges(seq(edge.label.start)) = edge
  }
}
