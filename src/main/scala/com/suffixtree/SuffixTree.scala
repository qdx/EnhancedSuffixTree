package com.suffixtree

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

object SuffixTree {
  val SEQ_END = -1
}

class SuffixTree[T](terminal: T) {
  val root = new Node[T](0)
  val sequence = new ArrayBuffer[T]()
  var ap = new ActivePoint[T](root, None, 0)
  var remainder_index = 0
  val future_node = new mutable.HashSet[Node[T]]

  def insert(i: T): Unit = {
    sequence.append(i)
//    val tmp_buffer = new ArrayBuffer[Node[T]]()
//    for (n <- future_node) {
//      val edge = n.edges(terminal)
//      n.edges.remove(terminal)
//      n.edges(i) = edge
//      tmp_buffer.append(n)
//    }
//    for (n <- tmp_buffer) {
//      future_node.remove(n)
//    }

    if (match_one_item(i)) {
      println("match success: " + i.toString)
      ap.edge_head = ap.edge_head.orElse(Some(i))
      ap.length += 1
      skip_edge()
    } else {
      println("match failed: " + i.toString)
      var previous_inserted_node = None: Option[Node[T]]
      println(s"\tremainder:$remainder_index, seq length: ${sequence.length}")
      // iterate over all possible suffixes
      for (k <- Range(remainder_index, sequence.length)) {
        println("\titeration: " + k)
        print("\t\t")
        show_ap()
        val new_node = insert_suffix(k, i)
        new_node match {
          case Some(node) =>
            previous_inserted_node match {
              case Some(pnode) =>
//                if(ap.node.type_ == Node.INTERNAL_NODE && ap.edge_head.isEmpty)
//                  pnode.suffix_link = Some(ap.node)
//                else
                pnode.suffix_link = Some(node)
                previous_inserted_node = Some(node)
              case None =>
                previous_inserted_node = Some(node)
            }
          case None =>
        }
        remainder_index += 1
      }
    }
  }

  def skip_edge(): Unit = {
    ap.edge_head match {
      case Some(head) =>
        val label = ap.node.edges(head).label
        if (label.end == label.start + ap.length - 1) {
          ap.node = ap.node.edges(ap.edge_head.get).to
          ap.edge_head = None
          ap.length = 0
        }
      case None => Unit
    }
    // Original was:
    // ==================== refactor into method ======================== //
    //    if (ap.length > 0) {
    //      val label = edge_label_getter(Some(i)).get
    //      if (label.end == label.start + ap.length - 1) {
    //        ap.node = ap.node.edges(ap.edge_head.get).to
    //        ap.edge_head = None
    //        ap.length = 0
    //      }
    //    }
    // =================================================================== //
  }

  def insert_suffix(begin_at: Int, input: T): Option[Node[T]] = {
    var new_node = None: Option[Node[T]]
    ap.edge_head match {
      case Some(head) =>
        assert(ap.length != 0)
        val ap_edge = ap.node.edges(head)
        if (sequence(ap_edge.label.start + ap.length).equals(input)) {
          if (ap_edge.label.start + ap.length == ap_edge.label.end) {
            val node = ap.node.edges(head).to
            println("\t\t\t node insert 1")
            new_node = node_insert(node, terminal, sequence.length, begin_at)
          } else {
            println("\t\t\t edge insert 1")
            new_node = Some(edge_insert(ap.node, head, ap.length, input, sequence.length, begin_at))
          }
        } else {
          println("\t\t\t edge insert 2")
          new_node = Some(edge_insert(ap.node, head, ap.length - 1, input, sequence.length - 1, begin_at))
        }
      case None =>
        assert(ap.length == 0)
        if (ap.node.edges.contains(input)) {
          println("\t\t\t edge insert 3")
          new_node = Some(edge_insert(ap.node, input, 0, terminal, sequence.length, begin_at))
        } else {
          println("\t\t\t node insert 2")
          new_node = node_insert(ap.node, input, sequence.length - 1, begin_at)
        }
    }
    move_active_point_after_split()
    new_node
  }

  def node_insert(node: Node[T], edge_head: T, label_start: Int, search_index: Int): Option[Node[T]] = {
    // create a new terminating edge
    val new_terminal_node = new Node[T](Node.LEAF_NODE, search_index)
    val new_edge = new Edge[T](label_start, SuffixTree.SEQ_END, new_terminal_node)
    // add the new edge to active node
    node.edges(edge_head) = new_edge
    if (edge_head.equals(terminal)) {
      future_node.add(node)
    }
    None
  }

  def edge_insert(node: Node[T], edge_head: T, split_point: Int, input: T, label_start: Int, search_index: Int): Node[T] = {
    // ----<name>---- stands for edges, O:<name> stands for node, then the following block of code
    // can be explained as:
    // ----ap_edge----O:ap_edge.to => ---ap_edge--- O:new_node ----new_edge---- O ap_edge.to
    //                                               \---new_terminal_edge----- O new terminal node
    val old_edge = node.edges(edge_head)
    val new_edge = new Edge[T](old_edge.label.start + split_point + 1, old_edge.label.end, old_edge.to)
    val new_node = new Node[T](Node.INTERNAL_NODE)

    old_edge.label = new Label(old_edge.label.start, old_edge.label.start + split_point)
    old_edge.to = new_node
    println("\t\t" + new_edge.label.toString)
    new_node.add_edge(sequence, new_edge)

    node_insert(new_node, input, label_start, search_index)
    new_node
  }

  def move_active_point_after_split(): Unit = {
    if (ap.node.type_ == Node.ROOT) {
      ap.edge_head match {
        case Some(head) =>
          ap.length -= 1
          if (ap.length == 0) ap.edge_head = None
          else ap.edge_head = Some(sequence(remainder_index + 1))
        case None => Unit
      }
    } else {
      ap.node.suffix_link match {
        case Some(item) =>
          val old_ap_label = ap.node.edges(ap.edge_head.get).label
          ap.node = item
          walk_down_ap(old_ap_label)
        case None => ap.node = root
      }
    }
    skip_edge()
  }

  def walk_down_ap(old_label: Label): Unit = {
    println("*** walk_down_ap called!***")
    ap.edge_head match {
      case Some(head) =>
        val ap_edge = ap.node.edges(head)
        if (ap.length > ap_edge.length(sequence.length)) {
          var slice = sequence.slice(old_label.start, old_label.start + ap.length)
          var new_active_edge = ap.node.edges(head)
          println("\tnew active edge length:" + new_active_edge.length(sequence.length))
          while (new_active_edge.length(sequence.length) < ap.length) {
            slice = slice.slice(new_active_edge.length(sequence.length), slice.length)
            ap.length -= new_active_edge.length(sequence.length)
            ap.node = ap.node.edges(ap.edge_head.get).to
            if (ap.length == 0) {
              ap.edge_head = None
            } else if (ap.length > 0) {
              ap.edge_head = Some(slice.head)
              new_active_edge = ap.node.edges(ap.edge_head.get)
            } else {
              println("active length should never be less than 0!")
              System.exit(-1)
            }
          }
        } else Unit
      case None => Unit
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
        println("node " + id_map(n) + " has " + n.edges.size + " children")
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
      print("Queue: ")
      queue.foreach((t: Node[T]) => print(id_map(t) + ", "))
      println()
      queue ++= add_queue
      for (i <- Range(0, s)) {
        queue.dequeue()
      }
    }

    println(sb)
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
