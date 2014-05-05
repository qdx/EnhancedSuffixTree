package qstream.automaton

import scala.collection.mutable

// TODO: use BigInt to replace some Int usage
object StreamObservation {

  case class OneObservation(state: Int, seq_num: Int)

}

class StreamObservation(window_size: Int, index_interval: Int) {

  var current_state = -1
  var window_counter = -1
  var index_head = -1
  // (state, seq_num)
  var observation_head = new StreamObservation.OneObservation(-1, -1)

  //HashMap[state, HashMap[seq_num, state]]
  val observation = new mutable.HashMap[Int, mutable.HashMap[Int, Int]]()

  //HashMap[seq_num, state]
  val time_index = new mutable.HashMap[Int, Int]()

  def input(ob: StreamObservation.OneObservation): Unit = {
    if (current_state == -1) current_state = ob.state
    else {
      // store new state
      if (!observation.contains(current_state)) {
        observation(current_state) = new mutable.HashMap[Int, Int]()
        // init observation head
        if (observation_head.state == -1) {
          observation_head = new StreamObservation.OneObservation(current_state, ob.seq_num)
        }
      }
      observation(current_state)(ob.seq_num) = ob.state
    }

    // update time index
    if (ob.seq_num % index_interval == 0) {
      time_index(ob.seq_num) = current_state
      // init index head
      if (index_head == -1) {
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
      if (tmp_oh.seq_num % index_interval == 0) {
        time_index.remove(index_head)
        index_head += index_interval
      }
      window_counter -= 1
    }

    current_state = ob.state
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
