package com.qdx.suffixtree

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import com.qdx.logging.Logger

object SuffixTree {
  val SEQ_END = -1
}

class SuffixTree[T] extends Logger {
  val sequence = new ArrayBuffer[T]()

  val root = new Node[T](0)
  var ap = new ActivePoint[T](root, None, 0)
  var remainder_index = 0

  var previous_inserted_node = None: Option[Node[T]]

  def batch_input(l: Iterable[T]): Unit = l.foreach((i: T) => insert(i))

  def insert(i: T): Unit = {
    sequence.append(i)

    var loop_flag = true
    var inserting = false

    while (loop_flag) {
      // try match the next item after active point with the input
      val match_result = match_one_item(i)
      if (match_result) {
        // match success, no need to insert anything, just move ap and setup suffix link
        ap.edge_head = ap.edge_head.orElse(Some(i))
        ap.length += 1
        establish_suffix_link(inserting, match_result, None)
        loop_flag = false
        walk_down_ap(ap.node.edges(ap.edge_head.get).label.start)
      } else {
        // when match failed, insert the suffixes from remainder_index till the end
        inserting = true
        val new_node = insert_suffix(remainder_index, i)
        loop_flag = remainder_index < sequence.length - 1
        establish_suffix_link(inserting, match_result, new_node)
        move_active_point_after_split()
        remainder_index += 1
      }
    }
  }

  // standard BFS traverse of the tree that returns a list of the nodes
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
    // used to do bfs
    val queue = new mutable.Queue[Node[T]]()
    queue.enqueue(root)

    // assign id to each node
    val id_map = new mutable.HashMap[Node[T], Int]()
    id_map(root) = 0

    // dispatch node id
    var id_counter = 1

    // building up the dot language described tree
    val sb = new StringBuilder(
      "\ndigraph suffixTree{\n node [shape=circle, label=\"\", fixedsize=true, width=0.1, height=0.1]\n")

    while (queue.length > 0) {
      val s = queue.length
      // temporary queue used to hold children nodes
      val add_queue = new mutable.Queue[Node[T]]()

      for (n <- queue) {
        for (e <- n.edges) {
          id_counter += {
            if (!id_map.contains(e._2.to)) {
              id_map(e._2.to) = id_counter
              1
            } else 0
          }
          sb.append(id_map(n)).append(" -> ").append(id_map(e._2.to)).append(" [label=\"")
          // getting the label of edge
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
      queue ++= add_queue
      Range(0, s).foreach(i => queue.dequeue())
    }

    sb.append("edge [color=red]\n")
    for ((k, v) <- id_map) {
      k.suffix_link match {
        case Some(node) => sb.append(id_map(k)).append(" -> ").append(id_map(node)).append(" ;\n")
        case None =>
      }
    }
    sb.append("}")
  }

  private def establish_suffix_link(inserting: Boolean, match_result: Boolean, new_node: Option[Node[T]]): Unit = {
    // we don't add suffix link for length 1 suffixes
    if (remainder_index >= sequence.length - 1) {
      previous_inserted_node = None
      return
    } else {
      // when we successfully matched the input item at the active point
      if (match_result) {
        // if we are in the middle of trying to insert multiple suffixes
        if (inserting) {
          previous_inserted_node match {
            case Some(pnode) =>
              // case 2: link between previously inserted new node and it's next suffix, this case
              // is similar to case 3, with the only difference is that in this case, no new node or
              // edge is inerted
              if (pnode.suffix_link.isEmpty && ap.node.type_ == Node.INTERNAL_NODE && !ap.node.equals(pnode)) {
                pnode.suffix_link = Some(ap.node)
              }
            case None =>
          }
          // such a successful match of input item means, current step comes to an end
          previous_inserted_node = None
        }
      } else {
        new_node match {
          case Some(node) =>
            previous_inserted_node match {
              case Some(pnode) =>
                // case 1: link between previously inserted new node and currently inserted new node
                if (pnode.suffix_link.isEmpty) {
                  pnode.suffix_link = Some(node)
                }
                previous_inserted_node = Some(node)
              case None =>
                previous_inserted_node = Some(node)
            }
          case None =>
            previous_inserted_node match {
              case Some(pnode) =>
                // case 3: when no new internal node is inserted, link between previously inserted new node and
                // the active node where terminal edge is inserted
                if (pnode.suffix_link.isEmpty && ap.node.type_ == Node.INTERNAL_NODE && !ap.node.equals(pnode)) {
                  pnode.suffix_link = Some(ap.node)
                }
              case None =>
            }
        }
      }
    }
  }

  // search index is the index we insert at leaf node, marking where the suffix gained by
  // concatenating from root to the this leaf starts at
  private def insert_suffix(search_index: Int, input: T): Option[Node[T]] = {
    var new_node = None: Option[Node[T]]
    ap.edge_head match {
      case Some(head) =>
        assert(ap.length != 0)
        new_node = Some(edge_insert(ap.node, head, ap.length - 1, input, sequence.length - 1, search_index))
      case None =>
        assert(ap.length == 0)
        node_insert(ap.node, input, sequence.length - 1, search_index)
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
    // ----old_edge----O:old_edge.to
    //      ||
    //      V
    // ---old_edge--- O:new_node ----new_edge---- O ap_edge.to
    //                 \---new_terminal_edge----- O new terminal node
    // while the new terminal_edge and node are inserted by node_insert
    val old_edge = node.edges(edge_head)
    val new_edge = new Edge[T](old_edge.label.start + split_point + 1, old_edge.label.end, old_edge.to)
    val new_node = new Node[T](Node.INTERNAL_NODE)

    old_edge.label = new Label(old_edge.label.start, old_edge.label.start + split_point)
    old_edge.to = new_node
    new_node.add_edge(sequence, new_edge)

    node_insert(new_node, input, label_start, search_index)
    new_node
  }

  private def move_active_point_after_split(): Unit = {
    // save the current ap edge label in case we need to walk down new ap
    val old_label = ap.edge_head match {
      case Some(head) => Some(ap.node.edges(head).label)
      case None => None
    }
    // when walking down new ap, if the previous ap is root, we start matching
    // items one item later than the case where previous ap is not root
    val offset = if (ap.node.type_ == Node.ROOT) 1 else 0
    if (ap.node.type_ == Node.ROOT) {
      // if we are at root, decrease ap length, move edge_head toward
      ap.edge_head match {
        case Some(head) =>
          ap.length -= 1
          if (ap.length == 0) ap.edge_head = None
          else ap.edge_head = Some(sequence(remainder_index + 1))
        case None => Unit
      }
    } else {
      // if not at root, follow suffix link if there is one, or return to root
      ap.node.suffix_link match {
        case Some(item) => ap.node = item
        case None => ap.node = root
      }
    }

    // walk down ap if ap length is greater than current active edge length
    if (old_label.isDefined
      && ap.edge_head.isDefined
      && ap.length >= ap.node.edges(ap.edge_head.get).length(sequence.length))
      walk_down_ap(old_label.get.start + offset)
  }

  // walk along paths when ap length is greater than current active edge
  private def walk_down_ap(start_at: Int): Unit = {
    ap.edge_head match {
      case Some(head) =>
        var cursor = 0
        var ap_edge = ap.node.edges(head)
        var ap_edge_len = ap_edge.length(sequence.length)
        while (ap.length != 0 && ap.length >= ap_edge_len) {
          ap.length -= ap_edge_len
          ap.node = ap_edge.to
          if (ap.length == 0) {
            ap.edge_head = None
          } else {
            val next_edge_head = sequence(start_at + cursor + ap_edge_len)
            cursor += ap_edge_len
            ap.edge_head = Some(next_edge_head)
            ap_edge = ap.node.edges(next_edge_head)
            ap_edge_len = ap_edge.length(sequence.length)
          }
        }
      case None => Unit
    }

  }

  private def match_one_item(i: T): Boolean = {
    ap.edge_head match {
      case None => ap.node.edges.contains(i)
      case Some(head) =>
        val edge_item_index = ap.node.edges(head).label.start + ap.length
        i.equals(sequence(edge_item_index))
    }
  }
}
