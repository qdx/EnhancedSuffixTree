package com.qdx.stream.automaton

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object StreamObservation {

  case class OneObservation(state: Int, seq_num: BigInt)

}

class StreamObservation(window_size: Int, index_interval: Int) {

  var current_state = -1
  var window_counter = -1
  var index_head = BigInt(-1)
  // (state, seq_num)
  var observation_head = new StreamObservation.OneObservation(-1, -1)

  //HashMap[state, HashMap[seq_num, state]]
  val observation = new mutable.HashMap[Int, mutable.HashMap[BigInt, Int]]()

  //HashMap[seq_num, state]
  val time_index = new mutable.HashMap[BigInt, Int]()

  def input(ob: StreamObservation.OneObservation): Unit = {
    if (current_state == -1) current_state = ob.state
    else {
      // init observation head
      if (observation_head.state == -1) {
        observation_head = new StreamObservation.OneObservation(current_state, ob.seq_num)
      }
      // store new state
      if (!observation.contains(current_state)) {
        observation(current_state) = new mutable.HashMap[BigInt, Int]()
      }
      observation(current_state)(ob.seq_num) = ob.state


      // update time index
      if (ob.seq_num % index_interval == BigInt(0)) {
        time_index(ob.seq_num) = current_state
        // init index head
        if (index_head == BigInt(-1)) {
          index_head = ob.seq_num
        }
      }

      // keep window size
      window_counter += 1
      if (window_counter >= window_size) {
        val tmp_oh = observation_head
        observation_head = new StreamObservation.OneObservation(observation(tmp_oh.state)(tmp_oh.seq_num), tmp_oh.seq_num + 1)
        observation(tmp_oh.state).remove(tmp_oh.seq_num)
        // remove out dated index
        if (tmp_oh.seq_num % index_interval == BigInt(0)) {
          time_index.remove(tmp_oh.seq_num)
          index_head = tmp_oh.seq_num + index_interval
        }
        window_counter -= 1
      }
    }
    current_state = ob.state
  }

  def walk_path(seq_num: BigInt, length: Int): ArrayBuffer[Int] = {
    if (seq_num < observation_head.seq_num) return new ArrayBuffer[Int]()
    val tmp = (seq_num / index_interval) * index_interval
    val index_seq = if (tmp < index_head) observation_head.seq_num else tmp
    val index_state = if (tmp < index_head) observation_head.state else time_index(index_seq)
    val result = new ArrayBuffer[Int]()

    var c_seq_num = index_seq
    var cstate = index_state
    var find_start = false
    while (!find_start) {
      if (c_seq_num == seq_num) find_start = true
      else {
        cstate = observation(cstate)(c_seq_num)
        c_seq_num += 1
      }
    }
    for (i <- 1 to length) {
      result.append(cstate)
      cstate = observation(cstate)(c_seq_num)
      c_seq_num += 1
    }
    result
  }

  def show_observation(): Unit = {
    for ((k, v) <- observation) {
      print(k)
      print(":" + v.mkString("  "))
      println()
    }
  }

  def show_index(): Unit = println(time_index.mkString("  "))

}
