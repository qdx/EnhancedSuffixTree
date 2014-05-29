package com.qdx.regex

import scala.collection.{mutable => m}
import scala.collection.mutable.ArrayBuffer

class State {
  var accept = false
  // ALTERNATE: change the value of hashmap into singleton instead of collection for dfa
  val to = new m.HashMap[Char, ArrayBuffer[State]]()

  def add_next(c: Char, s: State): Unit = {
    if (!to.contains(c)) {
      to(c) = new ArrayBuffer[State]()
    }
    to(c).append(s)
  }
}

class FiniteAutomaton {
  var start = new State
  var end = new State
}
