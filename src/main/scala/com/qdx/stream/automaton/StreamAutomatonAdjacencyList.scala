package com.qdx.stream.automaton

import scala.collection.mutable
import scala.util.Random
import com.qdx.suffixtree.SuffixTree
import com.qdx.suffixtree.Node
import com.qdx.logging.Logger
import com.qdx.regex.Pattern
import scala.util.matching.Regex

object StreamAutomatonAdjacencyList extends App {

  //  stream_automaton_test()
  //  transition_test()

  //  exact_path_search_test()
  //  special_suffix_tree_tests()
  //  manual_test_suffix_tree("abcabxabcd", '#')
  //  regex_search_test()

  val tree = new SuffixTree[Char]
  tree.batch_input("Today is a good day")
  println(tree.show())
  println("Input regex to query the suffix tree")
  while(true) {
    val input = Console.readLine()
    val p = new Pattern(input)
//    println(p.search_pattern(tree).mkString(", "))
  }


  def post_fix_test(): Unit = {
    val test_cases = Array(
      //      "abc"
      //      "(a.b).c"
      //      "(a\\b)c",
      //      "a(bc)",
      //      "ab|c",
      //      "ab+c",
      //      "a*bc",
      //      "asdf?",
      //      "a(b.c)+d?",
      //        "a*(b|cd?)+"
      //      "a(b\\c|d*e\\f)+|(gh)+"
      "ab(c|e|f)"
    )
    for (t <- Range(0, test_cases.length)) {
      val pattern = new Pattern(test_cases(t))
      println(pattern.show())
      //      println(nfa.show())
    }

  }

  def special_suffix_tree_tests(): Unit = {
    val test_cases = Array(
      "abcabxabcd",
      "dedododeeodo",
      "vvivvv",
      "bldfbdbdf",
      "mhmehmnhm",
      "qfqufquqfq",
      "wwtotwto",
      "knunuknuxknu"
    )
    for (c <- test_cases) {
      if (manual_test_suffix_tree(c, '#')) {
        println("test for:" + c + " passed")
      } else {
        println("test for:" + c + " failed")
      }
    }
  }

  def regex_search_test(): Unit = {
    val search_suffix_tree = new SuffixTree[Char]
    search_suffix_tree.log_level = Logger.ERROR
    val target = io.Source.fromURL(getClass.getResource("/summaTheologica.txt")).mkString + "~"
    search_suffix_tree.batch_input(target)

    val test_cases = Array("this", "what", "\\(", "(power|act) +of +a?", "(suf*icient|proximate)+,? and+")
    for (t <- test_cases) {
      val scala_regex = new Regex(t)
      val sr_match = scala_regex findAllMatchIn target
      val sr_set = new mutable.HashSet[(Int, Int)]()
      sr_match.foreach(m => sr_set.add((m.start, m.end - m.start)))
      val my_regex = new Pattern(t)
      val my_set = new mutable.HashSet[(Int, Int)]()
      val my_result = my_regex.search_pattern(search_suffix_tree)
      my_result.foreach(m => my_set.add(m))
      if (sr_set.equals(my_set)) {
        println(s"test case $t passed")
      } else {
        println(s"test case $t failed")
        println(s"scala has size:${sr_set.size}, mine has size:${my_set.size}")
        /* output test result to findout where is wrong
        val sf = new File("./scala_regex.txt")
        val sout = new PrintWriter(sf)
//        sr_set.foreach(m => sout.println(search_suffix_tree.sequence.slice(m._1, m._1 + m._2).mkString))
        sr_set.toArray.sorted.foreach(m => sout.println(s"(${m._1},${m._2})"))
        sout.close()
        val mf = new File("./my_regex.txt")
        val myout = new PrintWriter(mf)
//        my_set.foreach(m => myout.println(search_suffix_tree.sequence.slice(m._1, m._1 + m._2).mkString))
        my_set.toArray.sorted.foreach(m => myout.println(s"(${m._1},${m._2})"))
        myout.close()
        */
      }
    }
  }


  def exact_path_search_test(): Unit = {
    val search_suffix_tree = new SuffixTree[Char]
    search_suffix_tree.log_level = Logger.ERROR

    search_suffix_tree.batch_input(io.Source.fromURL(getClass.getResource("/summaTheologica.txt")).mkString + "~")
    //  println(search_suffix_tree.show())
    val search_result = search_suffix_tree.search("good")
    println(search_result.size)
    println(search_result.mkString(","))
  }

  def manual_test_suffix_tree[T](s: Iterable[T], terminal: T, log_level: Int = Logger.DEBUG): Boolean = {
    val test = new SuffixTree[T]
    test.log_level = log_level
    var counter = 0
    for (i <- s) {
      counter += 1
      test.insert(i)
      test.show()
    }
    test.insert(terminal)
    test.show()
    var leaf_c = 0
    for (n <- test.breadth_first_traverse()) {
      if (n.type_ == Node.LEAF_NODE) {
        leaf_c += 1
      }
    }
    if (leaf_c == counter + 1) true else false
  }

  def stream_automaton_test(): Unit = {
    val x = new StreamAutomatonAdjacencyList()
    val y = new StreamAutomatonAdjacencyMatrix()
    for (i <- 0 to 300) {
      val next_state = Random.nextInt(10)
      print(next_state + "->")
      x.input(State(next_state, System.currentTimeMillis()))
      y.input(State(next_state, System.currentTimeMillis()))
    }
    x.show()
    x.print_path()
    println(x.find_state(3).mkString)
    println(x.find_state(5).mkString)
    println(y.find_state(3).mkString)
    println(x.find_paths(3, 5))
    println(x.find_paths(3, 3))
    println(x.walk_path(7, 10).mkString(" -> "))
    y.show()

  }

  def transition_test(): Unit = {
    println("Starting!!!")
    val z = new StreamObservation(100, 13)
    val g = new TransitionGraph()
    for (i <- 0 to 300) {
      val next_state = Random.nextInt(10)
      print(next_state + "->")
      z.input(StreamObservation.OneObservation(next_state, i))
      g.input(next_state)
    }
    println("end of input")
    z.show_observation()
    z.show_index()

    g.show_graph()
    g.show_edge_weight()
    g.show_vertex_weight()
    g.show_path_record()

    println(z.observation_head)
    println(z.index_head)
    println(z.walk_path(205, 3).mkString(" "))
    println(z.walk_path(293, 3).mkString(" "))
    println(z.walk_path(290, 7).mkString(" "))
    println(z.walk_path(290, 3).mkString(" "))
    println(z.walk_path(259, 3).mkString(" "))
    println(z.walk_path(289, 3).mkString(" "))

  }
}

// To query state with time, build an index of states based on time
// interval that makes traversal fast enough
class StreamAutomatonAdjacencyList extends StreamAutomaton {
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

  def find_paths(state_src: Int, state_dst: Int, length: Int = -1): mutable.Queue[(Int, Int)] = {
    var src_i = 0
    var dst_i = 0
    val src_out_degree = automaton(state_src).length
    val dst_out_degree = automaton(state_dst).length
    val result = new mutable.Queue[(Int, Int)]
    while (src_i < src_out_degree && dst_i < dst_out_degree) {
      val seq_src = automaton(state_src)(src_i)._1
      val seq_dst = automaton(state_dst)(dst_i)._1
      if (seq_src < seq_dst) {
        if ((src_i + 1 < src_out_degree) && (automaton(state_src)(src_i + 1)._1 < seq_dst)) {
          src_i += 1
        }
        else {
          result.enqueue((seq_src, seq_dst))
          src_i += 1
          dst_i += 1
        }
      } else {
        dst_i += 1
      }
    }
    result
  }

  // ATTENTION: did not handle cases where length exceeds maximum value
  def walk_path(seq: Int, length: Int, state: Int = -1): Array[Int] = {
    var start = (-1, -1)
    if (state == -1) start = find_by_seq(seq)
    else start = (state, binary_find(seq, automaton(state)))
    var result = new Array[Int](0)
    var c_state = start._1
    var c_key = start._2
    var c_seq = seq
    for (i <- Range(0, length)) {
      result = result :+ c_state
      c_seq += 1
      c_state = automaton(c_state)(c_key)._2.state
      c_key = binary_find(c_seq, automaton(c_state))
    }
    result
  }

  //(state, index)
  def find_by_seq(seq: Int): (Int, Int) = {
    for ((k, v) <- automaton) {
      val key = binary_find(seq, v)
      if (key != -1 && v(key)._1 == seq) {
        return (k, key)
      }
    }
    (-1, -1)
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