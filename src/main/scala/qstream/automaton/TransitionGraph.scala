package qstream.automaton

import scala.collection.mutable

object TransitionGraph {

  case class OneTransition(from: Int, to: Int)

}

class TransitionGraph {

  val graph = new mutable.HashMap[Int, mutable.HashMap[Int, Int]]()
  val edge_weight = new mutable.HashMap[Int, mutable.HashMap[Int, Int]]()
  val path_record = new mutable.HashMap[Int, mutable.HashMap[Int, mutable.ArrayBuffer[Int]]]()
  val vertex_degree = new mutable.HashMap[Int, Int]()

  var current_state = -1

  def input(next_state: Int): Unit = {
    if (current_state == -1) current_state = next_state
    else {
      val row_update_queue = new mutable.Queue[TransitionGraph.OneTransition]()
      row_update_queue.enqueue(TransitionGraph.OneTransition(current_state, next_state))

      if (!graph.contains(current_state)) {
        graph(current_state) = new mutable.HashMap[Int, Int]()
      }

      // update weight
      if (!graph(current_state).contains(next_state) || graph(current_state)(next_state) != 1) {
        if (!vertex_degree.contains(current_state)) vertex_degree(current_state) = 1
        else vertex_degree(current_state) += 1
      }
      if (!edge_weight.contains(current_state)) {
        edge_weight(current_state) = new mutable.HashMap[Int, Int]()
      }
      if (!edge_weight(current_state).contains(next_state)) edge_weight(current_state)(next_state) = 1
      else edge_weight(current_state)(next_state) += 1

      // add new edge and
      if(safe_graph_lookup(current_state, next_state) != 1) {
        graph(current_state)(next_state) = 1

        val states = graph.keys

        // column update
        for (k <- states) {
          if (safe_graph_lookup(k, current_state) + 1 < safe_graph_lookup(k, next_state)) {
            if (safe_graph_lookup(k, next_state) != Double.PositiveInfinity)
              path_record_append(k, next_state, safe_graph_lookup(k, next_state).toInt)
            graph(k)(next_state) = graph(k)(current_state) + 1
            row_update_queue.enqueue(new TransitionGraph.OneTransition(k, next_state))
          }
        }

        // row update
        for (r <- row_update_queue) {
          for (k <- states) {
            if (safe_graph_lookup(r.to, k) + safe_graph_lookup(r.from, r.to) < safe_graph_lookup(r.from, k)) {
              if (safe_graph_lookup(r.from, k) != Double.PositiveInfinity)
                path_record_append(r.from, k, safe_graph_lookup(r.from, k).toInt)
              graph(r.from)(k) = graph(r.to)(k) + graph(r.from)(r.to)
            }
          }
        }
      }
      current_state = next_state
    }
  }

  def show_graph(): Unit = show("graph")

  def show_edge_weight(): Unit = show("weight")

  def show_vertex_weight(): Unit = println(vertex_degree.mkString("\n"))

  def show_path_record(): Unit = {
    val states = graph.keySet.toSeq.sorted
    for(f <- states){
      if(path_record.contains(f)){
        for(k <- path_record(f).keys){
          println(s"path: ($f, $k), length: " + path_record(f)(k).mkString(" "))
        }
      }
    }
  }

  private def show(which: String): Unit = {
    val states = graph.keySet.toSeq.sorted
    print("\n   \t")
    for (f <- states) {
      print(f + "\t")
    }
    println()
    for (f <- states) {
      print(f + ": \t")
      for (t <- states) {
        val dst = if (which.equals("graph")) safe_graph_lookup(f, t)
        else if (which.equals("weight")) safe_weight_lookup(f, t)
        else Double.PositiveInfinity
        val dst_str = if (dst == Double.PositiveInfinity) "NaN" else dst.toInt.toString
        print(dst_str + "\t")
      }
      println()
    }
  }

  private def safe_graph_lookup(from: Int, to: Int): Double = safe_lookup(from, to, graph)

  private def safe_weight_lookup(from: Int, to: Int): Double = safe_lookup(from, to, edge_weight)

  private def safe_lookup(i_1: Int, i_2: Int, data: mutable.HashMap[Int, mutable.HashMap[Int, Int]]): Double = {
    if (!data.contains(i_1)) scala.Double.PositiveInfinity
    else if (!data(i_1).contains(i_2)) scala.Double.PositiveInfinity
    else data(i_1)(i_2)
  }

  private def path_record_append(from: Int, to: Int, length: Int): Unit = {
    if (!path_record.contains(from))
      path_record(from) = new mutable.HashMap[Int, mutable.ArrayBuffer[Int]]()
    if (!path_record(from).contains(to))
      path_record(from)(to) = new mutable.ArrayBuffer[Int]()
    path_record(from)(to).append(length)
  }

}
