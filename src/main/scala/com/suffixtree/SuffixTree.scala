package com.suffixtree

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

object SuffixTree {
  val SEQ_END = -1
}

class SuffixTree[T] {
  val root = new Node[T](0)
  val sequence = new ArrayBuffer[T]()
  var ap = new ActivePoint[T](root, None, 0)
  var remainder_index = 0

  def insert(i: T): Unit = {
    sequence.append(i)
    if (match_one_item(i)) {
      println("match success: " + i.toString)
      val label = edge_label_getter(Some(i)).get
      if (label.end == label.start + ap.length) {
        ap.node = ap.node.edges(ap.edge_head.get).to
        ap.edge_head = None
        ap.length = 0
      } else {
        ap.length += 1
        ap.edge_head = ap.edge_head.orElse(Some(i))
      }
    } else {
      println("match failed: " + i.toString)
      var previous_inserted_node = None: Option[Node[T]]
      println(s"remainder:$remainder_index, seq length: ${sequence.length}")
      // iterate over all possible suffixes
      for (k <- Range(remainder_index, sequence.length)) {
        println("iteration: " + k)
        show_ap()
        if (ap.length == 0) {
          // create a new terminating edge
          val new_terminal_node = new Node[T](Node.LEAF_NODE, remainder_index)
          val new_edge = new Edge[T](sequence.length - 1, SuffixTree.SEQ_END, new_terminal_node)
          // add the new edge to active node
          ap.node.edges(sequence(new_edge.label.start)) = new_edge
          if (ap.node.type_ != Node.ROOT) {
            // rule 3: follow suffix if there is one, else go back to root
            ap.node.suffix_link match {
              case Some(item) => ap.node = item
              case None => ap.node = root
            }
          } else {
            ap.edge_head = None
          }
        } else {
          // ----<name>---- stands for edges, O:<name> stands for node, then the following block of code
          // can be explained as:
          // ----ap_edge----O:ap_edge.to => ---ap_edge--- O:new_node ----new_edge---- O ap_edge.to
          //                                               \---new_terminal_edge----- O new terminal node
          val ap_edge = ap.node.edges(ap.edge_head.get)
          ap_edge.to.type_ = Node.LEAF_NODE
          val ap_label = edge_label_getter(Some(i)).get
          val new_edge = new Edge[T](ap_label.start + ap.length, ap_label.end, ap_edge.to)
          val new_node = new Node[T](Node.INTERNAL_NODE)
          val new_terminal_node = new Node[T](Node.LEAF_NODE, remainder_index)
          val new_terminal_edge = new Edge[T](sequence.length - 1, SuffixTree.SEQ_END, new_terminal_node)

          new_node.add_edge(sequence, new_edge)
          ap_edge.to = new_node
          ap_edge.label = new Label(ap_edge.label.start, ap_edge.label.start + ap.length - 1)
          new_node.add_edge(sequence, new_terminal_edge)

          previous_inserted_node match {
            case Some(node) => node.suffix_link = Some(new_node)
            case None =>
          }
          if (ap.node.type_ == Node.ROOT) {
            ap.edge_head = Some(sequence(remainder_index + 1))
            ap.length -= 1
          } else {
            ap.node.suffix_link match {
              case Some(node) =>
                ap.node = node
              case None => ap.node = root
            }
          }
          previous_inserted_node = Some(new_node)
        }
        remainder_index += 1
      }
    }
  }

  def show_ap(): Unit = {
    println("Active Point:" + ap.node.type_ + ", " + ap.edge_head + ", " + ap.length)
  }

  def show(): Unit = {
    val queue = new mutable.Queue[Node[T]]()
    queue.enqueue(root)
    val id_map = new mutable.HashMap[Node[T], Int]()
    id_map(root) = 0
    var id_counter = 1
    val sb = new StringBuilder(
      "\ndigraph suffixTree{\n node [shape=circle, label=\"\", fixedsize=true, width=0.1, height=0.1]\n")

    while (queue.length > 0) {
      val s = queue.length
      val add_queue = new mutable.Queue[Node[T]]()
      for (n <- queue) {
        id_counter += safe_hash(n, id_map, id_counter)
        for (e <- n.edges) {
          id_counter += safe_hash(e._2.to, id_map, id_counter)
          sb.append(id_map(n)).append(" -> ").append(id_map(e._2.to)).append(" [label=\"")
          val end = if (e._2.label.end == -1) sequence.length - 1 else e._2.label.end
          val label = sequence.slice(e._2.label.start, end + 1)
          if (e._2.to.type_ != Node.LEAF_NODE) {
            add_queue.enqueue(e._2.to)
            sb.append(label.mkString).append("\"];\n")
          } else {
            sb.append(label.mkString).append("@" + e._2.to.search_index_).append("\"];\n")
          }
        }
      }
      println("Queue: ")
      queue.foreach((t: Node[T]) => print(id_map(t) + ", "))
      println()
      for (i <- Range(0, s)) {
        queue.dequeue()
      }
      queue ++= add_queue
    }

    sb.append("edge [color=red]\n")
    for ((k, v) <- id_map) {
      k.suffix_link match {
        case Some(node) => sb.append(id_map(k)).append(" -> ").append(id_map(node)).append(" ;\n")
        case None =>
      }
    }
    sb.append("}")
    println(sb)
    println("Active Point:" + id_map(ap.node) + ", " + ap.edge_head + ", " + ap.length)
    println(sequence.mkString)
  }

  private def safe_hash(key: Node[T], map: mutable.HashMap[Node[T], Int], id: Int): Int = {
    if (!map.contains(key)) {
      map(key) = id
      1
    }
    else 0
  }

  private def match_one_item(i: T): Boolean = {
    ap.edge_head match {
      case None => ap.node.edges.contains(i)
      case Some(item) =>
        val edge_item_index = edge_label_getter().get.start + ap.length
        i.equals(sequence(edge_item_index))
    }
  }

  private def edge_label_getter(t: Option[T] = None): Option[Label] = {
    ap.edge_head match {
      case None => Some(ap.node.edges(t.get).label)
      case Some(i) => Some(ap.node.edges(i).label)
    }
  }


}
