package qstream.automaton

import scala.collection.mutable
import scala.util.Random

object StreamAutomatonAdjacencyList extends App {
  val x = new StreamAutomatonAdjacencyList()
  val y = new StreamAutomatonAdjacencyMatrix()
  for (i <- Range(0, 20)) {
    val next_state = Random.nextInt(10)
    x.input(State(next_state, System.currentTimeMillis()))
    y.input(State(next_state, System.currentTimeMillis()))
  }
  x.show()
  x.print_path()
  println(x.find_state(3).mkString)
  println(y.find_state(3).mkString)
  //y.show()
}

// To query state with time, build an index of states based on time
// interval that makes traversal fast enough
class StreamAutomatonAdjacencyList extends StreamAutomaton{
  var current_state = 0
  var earliest_state = 0
  var stream_count = 0
  //                                 [state, queue((seq_num, state))]
  var automaton = new mutable.HashMap[Int, mutable.Queue[(Int, State)]]()
  automaton(0) = new mutable.Queue[(Int, State)]()

  // transfer : (target state: Int, time stamp: Int)
  def input(transfer: State): Unit = {
    if (!automaton.contains(transfer.state)) {
      automaton(transfer.state) = new mutable.Queue[(Int, State)]()
    }
    automaton(current_state).enqueue((stream_count, transfer))
    current_state = transfer.state
    stream_count += 1
  }

  def find_state(state: Int): mutable.Queue[(Int, State)] = {
    var result = new mutable.Queue[(Int, State)]
    result ++= automaton(state)
  }

  // return the index of the key, or the index of the smallest element that is greater than key
  def binary_find(k: Int, q: mutable.Queue[(Int, State)]): Int = {
    var order = true
    for (i <- Range(0, q.length - 1)) {
      order = order && (q(i)._1 < q(i + 1)._1)
    }
    require(order)

    binary_find_routine(k, q, 0, q.length - 1)
  }

  def binary_find_routine(k: Int, q: mutable.Queue[(Int, State)], left: Int, right: Int): Int = {
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