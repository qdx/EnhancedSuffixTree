package com.qdx.stream.automaton

import scala.collection.{mutable => m}

case class State(state:Int, time: Long)

trait StreamAutomaton {

  var current_state: Int
  var earliest_state: Int
  var stream_count: Int

  def input(transfer: State): Unit

  def find_state(state: Int): m.Queue[(Int, State)]

  def show(): Unit

}
