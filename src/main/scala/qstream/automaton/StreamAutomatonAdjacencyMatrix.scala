package qstream.automaton

import scala.collection.mutable

class StreamAutomatonAdjacencyMatrix extends StreamAutomaton {
  var current_state = 0
  var earliest_state = 0
  var stream_count = 0


  var automaton = new mutable.HashMap[Int, mutable.HashMap[Int, mutable.Queue[(Int, State)]]]
  automaton(0) = new mutable.HashMap[Int, mutable.Queue[(Int, State)]]()

  def input(transfer: State): Unit = {
    if (!automaton.contains(transfer.state)) {
      automaton(transfer.state) = new mutable.HashMap[Int, mutable.Queue[(Int, State)]]()
    }
    if (!automaton(current_state).contains(transfer.state)) {
      automaton(current_state)(transfer.state) = new mutable.Queue[(Int, State)]()
    }
    automaton(current_state)(transfer.state).enqueue((stream_count, transfer))
    current_state = transfer.state
    stream_count += 1
  }

  def find_state(state: Int): mutable.Queue[(Int, State)] = {
    var result = new mutable.Queue[(Int, State)]
    for((k, v) <- automaton(state)){
      result ++= automaton(state)(k)
    }
    result
  }

  def show(): Unit = {
    for ((k, v) <- automaton) {
      print(k + ":")
      for ((k_, v_) <- v) {
        print(v_.mkString)
      }
      println()
    }
  }
}
