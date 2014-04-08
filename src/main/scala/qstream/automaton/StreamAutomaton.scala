package qstream.automaton

import scala.collection.mutable
import scala.util.Random

object StreamAutomaton extends App {
  var stream_count = 0
  val x = new StreamAutomaton()
  for(i <- Range(0, 20)){
    val next_state = Random.nextInt(10)
    x.input((next_state, 1))
  }
  x.show()
  x.print_path()
}

class StreamAutomaton {
  var current_state = 0
  var earliest_state = 0
  //                                 [state,              seq_num, state, time]
  var automaton = new mutable.HashMap[Int, mutable.Queue[(Int, Int, Int)]]()
  automaton(0) = new mutable.Queue[(Int, Int, Int)]()

  // transfer : (target state: Int, time stamp: Int)
  def input(transfer: (Int, Int)): Unit = {
    if (!automaton.contains(transfer._1)) {
      automaton(transfer._1) = new mutable.Queue[(Int, Int, Int)]()
    }
    automaton(current_state).enqueue((StreamAutomaton.stream_count, transfer._1, transfer._2))
    current_state = transfer._1
    StreamAutomaton.stream_count += 1
  }

  // return the index of the key, or the index of the smallest element that is greater than key
  def binary_find(k: Int, q: mutable.Queue[(Int, Int, Int)]): Int = {
    var order = true
    for (i <- Range(0, q.length - 1)) {
      order = order && (q(i)._1 < q(i + 1)._1)
    }
    require(order)

    binary_find_routine(k, q, 0, q.length - 1)
  }

  def binary_find_routine(k: Int, q: mutable.Queue[(Int, Int, Int)], left: Int, right: Int): Int = {
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

  def print_path():Unit = {
    var cstate = 0
    var ctime = -1
    while (cstate != -1) {
      print(" " + cstate + " ->")
      val kindex = binary_find(ctime + 1, automaton(cstate))
      if (kindex == -1) cstate = -1
      else {
        val t = automaton(cstate)(kindex)
        cstate = t._2
        ctime = t._1
      }
    }
    println()
  }


}