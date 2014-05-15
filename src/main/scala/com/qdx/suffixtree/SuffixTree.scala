package com.qdx.suffixtree

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import com.qdx.logging.Logger

object SuffixTree {
  val SEQ_END = -1
}

class SuffixTree[T] extends Logger {
  val root = new Node[T](0)
  val sequence = new ArrayBuffer[T]()
  var ap = new ActivePoint[T](root, None, 0)
  var remainder_index = 0
  var previous_inserted_node = None: Option[Node[T]]

  def batch_input(l: Iterable[T]): Unit = l.foreach((i: T) => insert(i))

  def insert(i: T): Unit = {
    sequence.append(i)

    var loop_flag = true
    var inserting = false
    while (loop_flag) {
      if (match_one_item(i)) {
        debug("match success: " + i.toString)
        debug(get_status_string())
        ap.edge_head = ap.edge_head.orElse(Some(i))
        ap.length += 1
        debug(get_active_point_string())
        debug("\t\t\t remainder:" + remainder_index + " seq:" + sequence.length)
        debug("\t\t\t defined:" + previous_inserted_node.isDefined)
        debug("\t\t\t ap node:" + get_active_point_string())
        debug("\t\t\t inserting:" + inserting)
        if (previous_inserted_node.isDefined)
          debug("\t\t\t is the same:" + ap.node.equals(previous_inserted_node.get))
        establish_suffix_link(inserting, true, None)
        loop_flag = false
        skip_edge()
      } else {
        debug("match failed: " + i.toString)
        debug(get_status_string())
        inserting = true
        val new_node = insert_suffix(remainder_index, i)
        if (remainder_index >= sequence.length - 1) {
          loop_flag = false
        }
        debug("\t\t\t remainder:" + remainder_index + " seq:" + sequence.length)
        debug("\t\t\t defined:" + previous_inserted_node.isDefined)
        debug("\t\t\t ap node:" + get_active_point_string())
        debug("\t\t\t inserting:" + inserting)
        debug("\t\t\t previous node defined: " + previous_inserted_node.isDefined)
        establish_suffix_link(inserting, false, new_node)
        move_active_point_after_split()
        remainder_index += 1
      }
      debug(get_active_point_string())
    }
  }

  def breadth_first_traverse(): ArrayBuffer[Node[T]] = {
    val queue = new mutable.Queue[Node[T]]()
    val result = new mutable.ArrayBuffer[Node[T]]()
    queue.enqueue(root)
    while (queue.length > 0) {
      val s = queue.length
      val add_queue = new mutable.Queue[Node[T]]()
      for (n <- queue) {
        result.append(n)
        if (n.type_ != Node.LEAF_NODE)
          for (e <- n.edges) {
            add_queue.enqueue(e._2.to)
          }
      }
      queue ++= add_queue
      for (i <- Range(0, s)) {
        queue.dequeue()
      }
    }
    result
  }

  def get_status_string(): String = {
    s"\tremainder:$remainder_index, seq length: ${sequence.length}\n" + "\t\t" + get_active_point_string()
  }

  def get_active_point_string(): String = {
    "Active Point(" + ap.node.type_ + ", " + ap.edge_head + ", " + ap.length + ")"
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
        debug("node " + id_map(n) + " has " + n.edges.size + " children")
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
      debug("Queue: " + queue.map((t: Node[T]) => id_map(t)).mkString(", "))
      queue ++= add_queue
      for (i <- Range(0, s)) {
        queue.dequeue()
      }
    }

    sb.append("edge [color=red]\n")
    for ((k, v) <- id_map) {
      k.suffix_link match {
        case Some(node) => sb.append(id_map(k)).append(" -> ").append(id_map(node)).append(" ;\n")
        case None =>
      }
    }
    sb.append("}")
    debug(sb.toString())
    debug("Active Point(" + id_map(ap.node) + ", " + ap.edge_head + ", " + ap.length + ")")
    debug(sequence.mkString)
  }

  private def establish_suffix_link(inserting: Boolean, match_result: Boolean, new_node: Option[Node[T]]): Unit = {
    if (remainder_index >= sequence.length - 1) {
      previous_inserted_node = None
      return
    } else {
      if (match_result) {
        if (inserting) {
          previous_inserted_node match {
            case Some(pnode) =>
              if (pnode.suffix_link.isEmpty && ap.node.type_ == Node.INTERNAL_NODE && !ap.node.equals(pnode)) {
                debug("\t\t suffix link inserted")
                pnode.suffix_link = Some(ap.node)
              }
            case None =>
          }
          previous_inserted_node = None
        }
      } else {
        new_node match {
          case Some(node) =>
            previous_inserted_node match {
              case Some(pnode) =>
                if (pnode.suffix_link.isEmpty) {
                  debug("\t\t suffix link inserted")
                  pnode.suffix_link = Some(node)
                }
                previous_inserted_node = Some(node)
              case None =>
                previous_inserted_node = Some(node)
            }
          case None =>
            previous_inserted_node match {
              case Some(pnode) =>
                if (pnode.suffix_link.isEmpty && ap.node.type_ == Node.INTERNAL_NODE && !ap.node.equals(pnode)) {
                  debug("\t\t suffix link inserted")
                  pnode.suffix_link = Some(ap.node)
                }
              case None =>
            }
        }
      }
    }
  }

  private def skip_edge(): Unit = {
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
  }

  private def insert_suffix(begin_at: Int, input: T): Option[Node[T]] = {
    var new_node = None: Option[Node[T]]
    ap.edge_head match {
      case Some(head) =>
        assert(ap.length != 0)
        val ap_edge = ap.node.edges(head)
        if (sequence(ap_edge.label.start + ap.length).equals(input)) {
          debug("\t\t\t edge insert 1")
          new_node = Some(edge_insert(ap.node, head, ap.length, input, sequence.length, begin_at))
        } else {
          debug("\t\t\t edge insert 2")
          new_node = Some(edge_insert(ap.node, head, ap.length - 1, input, sequence.length - 1, begin_at))
        }
      case None =>
        assert(ap.length == 0)
        debug("\t\t\t node insert 2")
        node_insert(ap.node, input, sequence.length - 1, begin_at)
    }
    new_node
  }

  private def node_insert(node: Node[T], edge_head: T, label_start: Int, search_index: Int): Unit = {
    // create a new terminating edge
    val new_terminal_node = new Node[T](Node.LEAF_NODE, search_index)
    val new_edge = new Edge[T](label_start, SuffixTree.SEQ_END, new_terminal_node)
    // add the new edge to active node
    node.edges(edge_head) = new_edge
  }

  private def edge_insert(node: Node[T], edge_head: T, split_point: Int, input: T, label_start: Int, search_index: Int): Node[T] = {
    // ----<name>---- stands for edges, O:<name> stands for node, then the following block of code
    // can be explained as:
    // ----ap_edge----O:ap_edge.to => ---ap_edge--- O:new_node ----new_edge---- O ap_edge.to
    //                                               \---new_terminal_edge----- O new terminal node
    val old_edge = node.edges(edge_head)
    val new_edge = new Edge[T](old_edge.label.start + split_point + 1, old_edge.label.end, old_edge.to)
    val new_node = new Node[T](Node.INTERNAL_NODE)

    old_edge.label = new Label(old_edge.label.start, old_edge.label.start + split_point)
    old_edge.to = new_node
    debug("\t\t" + new_edge.label.toString)
    new_node.add_edge(sequence, new_edge)

    node_insert(new_node, input, label_start, search_index)
    new_node
  }

  private def move_active_point_after_split(): Unit = {
    val old_label = ap.edge_head match {
      case Some(head) => Some(ap.node.edges(head).label)
      case None => None
    }
    val offset = if (ap.node.type_ == Node.ROOT) 1 else 0
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
        case Some(item) => ap.node = item
        case None => ap.node = root
      }
    }
    old_label match {
      case Some(label) =>
        debug(get_active_point_string())
        walk_down_ap(label.start + offset)
        debug(get_active_point_string())
      case None =>
    }
    skip_edge()
  }

  private def walk_down_ap(start_at: Int): Unit = {
    info("walk_down_ap called!")
    ap.edge_head match {
      case Some(head) =>
        info("edge_head defined")
        var ap_edge = ap.node.edges(head)
        var ap_edge_len = ap_edge.length(sequence.length)
        var cursor = 0
        info(get_active_point_string())
        info("ap edge label:" + ap_edge.label)
        info("ap length:" + ap.length + " ap edge length:" + ap_edge_len)
        while (ap.length != 0 && ap.length >= ap_edge_len) {
          val next_edge_head = sequence(start_at + cursor + ap_edge_len)
          cursor += ap_edge_len
          info("in while loop, next edge head is:" + next_edge_head)
          ap.length -= ap_edge_len
          ap.node = ap_edge.to
          if (ap.length == 0) {
            ap.edge_head = None
          } else {
            ap.edge_head = Some(next_edge_head)
            ap_edge = ap.node.edges(next_edge_head)
            ap_edge_len = ap_edge.length(sequence.length)
          }
        }
      case None => Unit
    }

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
