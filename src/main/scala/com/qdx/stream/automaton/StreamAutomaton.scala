package com.qdx.stream.automaton

import scala.collection.mutable

case class State(state:Int, time: Long)

trait StreamAutomaton {

  var current_state: Int
  var earliest_state: Int
  var stream_count: Int

  def input(transfer: State): Unit

  def find_state(state: Int): mutable.Queue[(Int, State)]

  def show(): Unit

}
