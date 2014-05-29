package com.qdx.stream.automaton

import scala.collection.{mutable => m}

class StreamAutomatonAdjacencyHash extends StreamAutomaton{

  var current_state = 0
  var earliest_state = 0
  var stream_count = 0

  //                                  [state, hash(seq_num, state)]
  var automaton = new m.HashMap[Int, m.HashMap[Int, State]]()
  automaton(0) = new m.HashMap[Int, State]()

  def input(transfer: State): Unit = {


  }

  def find_state(state: Int): m.Queue[(Int, State)] = {

    new m.Queue[(Int, State)]()
  }

  def show(): Unit = {

  }

}
