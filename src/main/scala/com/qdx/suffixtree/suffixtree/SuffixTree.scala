package com.qdx.suffixtree.suffixtree

import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable => m}
import com.qdx.debugging.Logger

object SuffixTree {
  val SEQ_END = BigInt(-1)
}

class InternalNodeNumOfChildrenException extends Exception {}

// Implementing suffix tree using Ukkonen's algorithm, great help from:
// http://stackoverflow.com/questions/9452701/ukkonens-suffix-tree-algorithm-in-plain-english
class SuffixTree[T] extends Logger {
  log_level = Logger.ERROR
  val sequence = new ArrayBuffer[T]
  val leaves = new ArrayBuffer[Option[Node[T]]]
  var window_head = 0: BigInt
  var window_size = 0: Int
  var slide_size = 1: Int

  val root = new Node[T](0)
  var ap = new ActivePoint[T](root, None, 0)
  var remainder_index = 0: Int

  var previous_inserted_node = None: Option[Node[T]]

  var repeated = None: Option[SuffixTree[T]]

  def get_height(): Int = {
    if (repeated.isDefined) repeated.get.get_height() + 1
    else 0
  }

  def batch_input(l: Iterable[T]): Unit = l.foreach((i: T) => insert(i))

  def insert(i: T): Unit = {
    sequence.append(i)
    leaves.append(None)

    var loop_flag = true
    var inserting = false

    while (loop_flag) {
      // try match the next item after active point with the input
      val match_result = match_one_item(i)
      if (match_result) {
        // match success, no need to insert anything, just move ap and setup suffix link
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
        establish_suffix_link(inserting, match_result, None)
        loop_flag = false
        walk_down_ap((ap.node.edges(ap.edge_head.get).label.start - window_head).toInt)
        if (repeated.isEmpty) {
          repeated = Some(new SuffixTree[T])
          //          repeated.get.log_level = Logger.DEBUG
        }
        repeated.get.insert(i)
      } else {
        // when match failed, insert the suffixes from remainder_index till the end
        debug("match failed: " + i.toString)
        debug(get_status_string())
        inserting = true
        val new_node = insert_suffix(remainder_index, i)
        debug("\t\t\t remainder:" + remainder_index + " seq:" + sequence.length)
        debug("\t\t\t defined:" + previous_inserted_node.isDefined)
        debug("\t\t\t ap node:" + get_active_point_string())
        debug("\t\t\t inserting:" + inserting)
        debug("\t\t\t previous node defined: " + previous_inserted_node.isDefined)
        establish_suffix_link(inserting, match_result, new_node)
        move_active_point_after_split()
        loop_flag = remainder_index < sequence.length - 1
        remainder_index += 1
        if (repeated.isDefined) {
          repeated.get.slide()
          if (repeated.get.sequence.length == 0) repeated = None
        }
      }
      debug(get_active_point_string())
      debug(show(label_as_item = false))
    }
    slide()
  }

  // standard BFS traverse of the tree that returns a list of the nodes
  def breadth_first_traverse(root_node: Node[T] = root): ArrayBuffer[Node[T]] = {
    val result = new m.ArrayBuffer[Node[T]]()
    val queue = new m.Queue[Node[T]]()
    queue.enqueue(root_node)
    while (queue.length > 0) {
      val s = queue.length
      val add_queue = new m.Queue[Node[T]]()
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

  // this method is deprecated, it searches for exact string in the suffix tree
  def search(s: Iterable[T]): ArrayBuffer[BigInt] = {
    val result = new ArrayBuffer[BigInt]()
    val matching_point = new ActivePoint[T](root, None, 0)
    for (i <- s) {
      matching_point.edge_head match {
        case Some(head) =>
          val mp_edge = matching_point.node.edges(head)
          if (!i.equals(sequence((mp_edge.label.start - window_head).toInt + matching_point.length))) {
            return result
          }
        case None =>
          if (!matching_point.node.edges.contains(i)) {
            return result
          }
      }
      matching_point.edge_head = matching_point.edge_head.orElse(Some(i))
      val mp_edge = matching_point.node.edges(matching_point.edge_head.get)
      matching_point.length += 1
      if (matching_point.length == mp_edge.length(sequence.length, window_head)) {
        matching_point.node = mp_edge.to
        matching_point.edge_head = None
        matching_point.length = 0
      }
    }
    val terminating_at = matching_point.edge_head match {
      case Some(head) => matching_point.node.edges(head).to
      case None => matching_point.node
    }
    for (n <- breadth_first_traverse(terminating_at))
      if (n.type_ == Node.LEAF_NODE) result.append(n.search_index_)
    result
  }

  def get_status_string(): String = {
    s"\tremainder:$remainder_index, seq length: ${sequence.length}\n" + "\t\t" + get_active_point_string()
  }

  def get_active_point_string(): String = {
    "Active Point(" + ap.node.type_ + ", " + ap.edge_head + ", " + ap.length + ")"
  }

  def show(back_link: Boolean = false, label_as_item: Boolean = true): String = {
    // used to do bfs
    val queue = new m.Queue[Node[T]]()
    queue.enqueue(root)

    // assign id to each node
    val id_map = new m.HashMap[Node[T], Int]()
    id_map(root) = 0

    // dispatch node id
    var id_counter = 1

    // building up the dot language described tree
    val sb = new StringBuilder(
      "\ndigraph suffixTree{\n node [shape=circle, label=\"\", fixedsize=true, width=0.1, height=0.1]\n")

    while (queue.length > 0) {
      val s = queue.length
      // temporary queue used to hold children nodes
      val add_queue = new m.Queue[Node[T]]()

      for (n <- queue) {
        debug("node " + id_map(n) + " has " + n.edges.size + " children")
        for (e <- n.edges.values) {
          id_counter += {
            if (!id_map.contains(e.to)) {
              id_map(e.to) = id_counter
              1
            } else 0
          }
          sb.append(id_map(n)).append(" -> ").append(id_map(e.to)).append(" [label=\"")
          // getting the label of edge

          val label_index = s"(${e.label.start}, ${e.label.end}})"
          val label =
            if (label_as_item) e.get_label_seq(sequence, window_head).mkString
            else e.get_label_seq(sequence, window_head).mkString + label_index

          if (e.to.type_ != Node.LEAF_NODE) {
            add_queue.enqueue(e.to)
            sb.append(label).append("\"];\n")
          } else {
            sb.append(label).append("@" + (e.to.search_index_ - window_head)).append("\"];\n")
            if (back_link) {
              val ln = e.to
              sb.append(id_map(ln)).append(" -> ").append(id_map(ln.from_edge.get.from)).append(" ;\n")
            }
          }
        }
        if (n.from_edge.isDefined && back_link) {
          sb.append(id_map(n)).append(" -> ").append(id_map(n.from_edge.get.from)).append(" ;\n")
        }
      }
      debug("Queue: " + queue.map((t: Node[T]) => id_map(t)).mkString(", "))
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
    //    debug(sb.toString())
    debug("Active Point(" + id_map(ap.node) + ", " + ap.edge_head + ", " + ap.length + ")")
    debug(sequence.mkString)
    sb.toString()
  }

  def get_item(i: BigInt): T = {
    sequence((i - window_head).toInt)
  }

  // test equality in a relative weak sense
  def equals(t: SuffixTree[T]): Boolean = {
    if (sequence.equals(t.sequence)) {
      val thisroot = root
      val thatroot = t.root
      dfs_equals(thisroot, thatroot, this, t)
    } else false
  }

  private def dfs_equals(n1: Node[T], n2: Node[T], t1: SuffixTree[T], t2: SuffixTree[T]): Boolean = {
    if (!n1.edges.keySet.equals(n2.edges.keySet)) false
    else {
      // ALTERNATE: to make the implementation easier, only test existence here
      (n1.suffix_link.isDefined == n2.suffix_link.isDefined) &&
        n1.edges.forall(x =>
          n2.edges(x._1).get_label_seq(t2.sequence, t2.window_head)
            .equals(x._2.get_label_seq(t1.sequence, t1.window_head))
            && dfs_equals(x._2.to, n2.edges(x._1).to, t1, t2)
        )
    }
  }

  def set_slide_size(size: Int): Unit = slide_size = size

  def slide(): Unit = {
    while(sequence.length > window_size + slide_size){
      Range(0, slide_size).foreach(_ => slide_one())
    }
  }

  private def slide_one(): Unit = {
    if (leaves.size == 0) return
    // find the leaf node that represents the suffix we are deleting
    val n = leaves.head.get
    // find the parent of that leaf node
    val np = n.get_parent_node().get
    // the edge from np to n
    val e = n.from_edge.get

    debug(n.from_edge.get.get_label_seq(sequence, window_head).mkString(","))
    if (np.edges.size == 1 && np.type_ == Node.ROOT) {
      sequence.clear()
      leaves.clear()
      window_head = 0: BigInt
      window_size = 0: Int
      root.edges.clear()
      ap = new ActivePoint[T](root, None, 0)
      remainder_index = 0: Int
      previous_inserted_node = None: Option[Node[T]]
      repeated = None: Option[SuffixTree[T]]
    } else {
      if (ap.node.equals(np) && ap.edge_head.isDefined && ap.get_edge().equals(n.from_edge)) {
        // when ap is on the edge that is leading to the leaf node we are looking at, we need to
        // insert the suffix indicated by remainder index
        e.label = new Label((remainder_index + (e.label.start - (n.search_index_ - window_head))).toInt, e.label.end)
        n.search_index_ = remainder_index + window_head
        debug("relabeling edge and leaf")
        leaves(remainder_index) = Some(n)
        bubble_up_label_change(np, e.label.start)
        move_active_point_after_split()
      } else {
        // we don't need to insert any suffix, but we do need to remove the leaf we are looking at
        if (np.edges.size == 2 && np.type_ != Node.ROOT) {

          // get the other node
          val other_edge_head = (np.edges.keySet - get_item(n.from_edge.get.label.start)).head
          val o = np.edges(other_edge_head).to
          val npp = np.get_parent_node().get
          // in this case we need to merge edges
          // to make it clearer, here is a graph:
          // -------> npp --------------------------------> np --------> n (we need to remove this leaf)
          //           \ we don't care about this subtree    \---------> o (we don't care about this subtree)
          // after the operation, we want it to look like
          // -------> npp -------------> o (we don't care)
          //           \ we don't care
          if (ap.node.equals(np)) {
            debug("adjust ap")
            // when ap is in the subtree we are manipulating, we have to move ap to the correct position
            ap.node = npp
            ap.edge_head = Some(get_item(np.from_edge.get.label.start))
            ap.length = np.from_edge.get.length(sequence.length, window_head) + ap.length
          }
          bubble_up_label_change(np, o.from_edge.get.label.start)
          val np_edge = np.from_edge.get
          // merging edges
          debug("merging edges")
          np_edge.label = new Label(np_edge.label.start, o.from_edge.get.label.end)
          np_edge.to = o
          o.from_edge = np.from_edge
        } else if (np.edges.size >= 2 || np.type_ == Node.ROOT) {
          debug("simply remove edge")
          // in this case, we just need to remove the leaf and it's from edge
          np.edges.remove(get_item(n.from_edge.get.label.start))
          bubble_up_label_change(np, np.edges.values.head.label.start)
        } else {
          // any internal node should have at least 2 edges, in this branch this is violated
          throw new InternalNodeNumOfChildrenException
        }
        // since we have slided the sequence backwards, in order to keep remainder index at the right place,
        // we need to slide it backwards too.
        remainder_index -= 1
      }
      sequence.remove(0)
      leaves.remove(0)
      window_head += 1
    }
    debug(show(label_as_item = false))
  }

  private def bubble_up_label_change(starting: Node[T], base: BigInt): Unit = {
    var n = starting
    var b = base
    while (n.type_ != Node.ROOT) {
      val from_edge = n.from_edge.get
      val length = from_edge.length(sequence.length, window_head)
      from_edge.label = new Label(b - length, b - 1)
      b = from_edge.label.start
      n = from_edge.from
    }
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
                debug("\t\t suffix link inserted")
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
                // case 3: when no new internal node is inserted, link between previously inserted new node and
                // the active node where terminal edge is inserted
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

  // search index is the index we insert at leaf node, marking where the suffix gained by
  // concatenating from root to the this leaf starts at
  private def insert_suffix(search_index: BigInt, input: T): Option[Node[T]] = {
    var new_node = None: Option[Node[T]]
    ap.edge_head match {
      case Some(head) =>
        assert(ap.length != 0)
        debug("\t\t\t edge insert")
        new_node = Some(edge_insert(ap.node, head, ap.length - 1, input, sequence.length - 1 + window_head, search_index))
      case None =>
        assert(ap.length == 0)
        debug("\t\t\t node insert")
        node_insert(ap.node, input, sequence.length - 1 + window_head, search_index)
    }
    new_node
  }

  private def node_insert(node: Node[T], edge_head: T, label_start: BigInt, search_index: BigInt): Unit = {
    // create a new terminating edge
    val new_terminal_node = new Node[T](Node.LEAF_NODE, window_head + search_index)
    val new_edge = new Edge[T](label_start, SuffixTree.SEQ_END, node, new_terminal_node)
    // add the new edge to active node
    node.edges(edge_head) = new_edge
    // link back to parent
    new_terminal_node.from_edge = Some(new_edge)
    // add the terminal node to leaf reference
    leaves(remainder_index) = Some(new_terminal_node)
  }

  private def edge_insert(node: Node[T], edge_head: T, split_point: Int, input: T, label_start: BigInt, search_index: BigInt): Node[T] = {
    // ----<name>---- stands for edges, O:<name> stands for node, then the following block of code
    // can be explained as:
    // ----old_edge----O:old_edge.to
    //      ||
    //      V
    // ---old_edge--- O:new_node ----new_edge---- O old_edge.to
    //                 \---new_terminal_edge----- O new terminal node
    // while the new terminal_edge and node are inserted by node_insert
    val old_edge = node.edges(edge_head)
    val new_node = new Node[T](Node.INTERNAL_NODE)
    val new_edge = new Edge[T](old_edge.label.start + split_point + 1, old_edge.label.end, new_node, old_edge.to)
    old_edge.to.from_edge = Some(new_edge)

    old_edge.label = new Label(old_edge.label.start, old_edge.label.start + split_point)
    old_edge.to = new_node
    new_node.from_edge = Some(old_edge)
    debug("\t\t" + new_edge.label.toString)
    new_node.add_edge(sequence, window_head, new_edge)

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
    // TODO: I really forgot why I need this offset here
    val offset = if (ap.node.type_ == Node.ROOT) 1 else 0
    if (ap.node.type_ == Node.ROOT) {
      // if we are at root, decrease ap length, move edge_head forward
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

    debug(get_active_point_string())
    // walk down ap if ap length is greater than current active edge length
    if (old_label.isDefined
      && ap.edge_head.isDefined
      && ap.length >= ap.node.edges(ap.edge_head.get).length(sequence.length, window_head))
      walk_down_ap((old_label.get.start + offset - window_head).toInt)
    debug(get_active_point_string())
  }

  // walk along paths when ap length is greater than current active edge
  private def walk_down_ap(start_at: Int): Unit = {
    info("walk_down_ap called!")
    ap.edge_head match {
      case Some(head) =>
        info("edge_head defined")
        var cursor = 0
        var ap_edge = ap.node.edges(head)
        var ap_edge_len = ap_edge.length(sequence.length, window_head)
        info(get_active_point_string())
        info("ap edge label:" + ap_edge.label)
        info("ap length:" + ap.length + " ap edge length:" + ap_edge_len)
        while (ap.length != 0 && ap.length >= ap_edge_len) {
          ap.length -= ap_edge_len
          ap.node = ap_edge.to
          if (ap.length == 0) {
            ap.edge_head = None
          } else {
            val next_edge_head = sequence(start_at + cursor + ap_edge_len)
            cursor += ap_edge_len
            info("in while loop, next edge head is:" + next_edge_head)
            ap.edge_head = Some(next_edge_head)
            ap_edge = ap.node.edges(next_edge_head)
            ap_edge_len = ap_edge.length(sequence.length, window_head)
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
        i.equals(get_item(edge_item_index))
    }
  }
}
