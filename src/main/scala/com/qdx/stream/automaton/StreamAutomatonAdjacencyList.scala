package com.qdx.stream.automaton

import scala.collection.{mutable => m}

object StreamAutomatonAdjacencyList{
}

// To query state with time, build an index of states based on time
// interval that makes traversal fast enough
class StreamAutomatonAdjacencyList extends StreamAutomaton {
  var current_state = 0
  var earliest_state = 0
  var stream_count = 0
  //                                 [state, queue((seq_num, state))]
  var automaton = new m.HashMap[Int, m.Queue[(Int, State)]]()
  automaton(0) = new m.Queue[(Int, State)]()

  // transfer : (target state: Int, time stamp: Int)
  def input(transfer: State): Unit = {
    if (!automaton.contains(transfer.state)) {
      automaton(transfer.state) = new m.Queue[(Int, State)]()
    }
    automaton(current_state).enqueue((stream_count, transfer))
    current_state = transfer.state
    stream_count += 1
  }

  def find_state(state: Int): m.Queue[(Int, State)] = {
    var result = new m.Queue[(Int, State)]
    result ++= automaton(state)
  }

  def find_paths(state_src: Int, state_dst: Int, length: Int = -1): m.Queue[(Int, Int)] = {
    var src_i = 0
    var dst_i = 0
    val src_out_degree = automaton(state_src).length
    val dst_out_degree = automaton(state_dst).length
    val result = new m.Queue[(Int, Int)]
    while (src_i < src_out_degree && dst_i < dst_out_degree) {
      val seq_src = automaton(state_src)(src_i)._1
      val seq_dst = automaton(state_dst)(dst_i)._1
      if (seq_src < seq_dst) {
        if ((src_i + 1 < src_out_degree) && (automaton(state_src)(src_i + 1)._1 < seq_dst)) {
          src_i += 1
        }
        else {
          result.enqueue((seq_src, seq_dst))
          src_i += 1
          dst_i += 1
        }
      } else {
        dst_i += 1
      }
    }
    result
  }

  // ATTENTION: did not handle cases where length exceeds maximum value
  def walk_path(seq: Int, length: Int, state: Int = -1): Array[Int] = {
    var start = (-1, -1)
    if (state == -1) start = find_by_seq(seq)
    else start = (state, binary_find(seq, automaton(state)))
    var result = new Array[Int](0)
    var c_state = start._1
    var c_key = start._2
    var c_seq = seq
    for (i <- Range(0, length)) {
      result = result :+ c_state
      c_seq += 1
      c_state = automaton(c_state)(c_key)._2.state
      c_key = binary_find(c_seq, automaton(c_state))
    }
    result
  }

  //(state, index)
  def find_by_seq(seq: Int): (Int, Int) = {
    for ((k, v) <- automaton) {
      val key = binary_find(seq, v)
      if (key != -1 && v(key)._1 == seq) {
        return (k, key)
      }
    }
    (-1, -1)
  }

  // return the index of the key, or the index of the smallest element that is greater than key
  def binary_find(k: Int, q: m.Queue[(Int, State)]): Int = {
    var order = true
    for (i <- Range(0, q.length - 1)) {
      order = order && (q(i)._1 < q(i + 1)._1)
    }
    require(order)

    binary_find_routine(k, q, 0, q.length - 1)
  }

  def binary_find_routine(k: Int, q: m.Queue[(Int, State)], left: Int, right: Int): Int = {
    if (left > right) {
      return -1
    }
    val pivot = (left + right) / 2
    if (k < q(pivot)._1) binary_find_routine(k, q, left, pivot - 1)
    else if (k > q(pivot)._1) binary_find_routine(k, q, pivot + 1, right)
    else pivot
  }

  def show(): Unit = {
    for ((k, v) <- automaton) {
      print(k)
      print(":" + v.mkString)
      println()
    }
  }

  def print_path(): Unit = {
    var cstate = 0
    var ctime = -1
    while (cstate != -1) {
      print(" " + cstate + " ->")
      val kindex = binary_find(ctime + 1, automaton(cstate))
      if (kindex == -1) cstate = -1
      else {
        val t = automaton(cstate)(kindex)
        cstate = t._2.state
        ctime = t._1
      }
    }
    println()
  }
}