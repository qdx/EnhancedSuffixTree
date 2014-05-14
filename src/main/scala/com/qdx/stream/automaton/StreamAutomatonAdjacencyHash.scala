package com.qdx.stream.automaton

import scala.collection.mutable

class StreamAutomatonAdjacencyHash extends StreamAutomaton{

  var current_state = 0
  var earliest_state = 0
  var stream_count = 0

  //                                  [state, hash(seq_num, state)]
  var automaton = new mutable.HashMap[Int, mutable.HashMap[Int, State]]()
  automaton(0) = new mutable.HashMap[Int, State]()

  def input(transfer: State): Unit = {


  }

  def find_state(state: Int): mutable.Queue[(Int, State)] = {

    new mutable.Queue[(Int, State)]()
  }

  def show(): Unit = {

  }

}
