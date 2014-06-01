package com.qdx.demo

import scala.collection.{mutable => m}
import scala.util.Random
import com.qdx.suffixtree.{SuffixTreeActor, SuffixTree, Node}
import com.qdx.logging.Logger
import com.qdx.regex.Pattern
import com.qdx.stream.automaton._
import scala.util.matching.Regex
import scala.concurrent.duration._
import java.io.File
import akka.actor.{Props, ActorSystem}

object MainEntry extends App {

  //  stream_automaton_test()
  //  transition_test()

  //  exact_path_search_test()
  //  special_suffix_tree_tests()
  //  manual_test_suffix_tree("abcabxabcd", '#')
//    regex_search_test()

//  concurrent_demo()
  val tree = new SuffixTree[Char]
  tree.log_level = Logger.DEBUG
  tree.batch_input("cvjvj")
  println(tree.show())
//  println(tree.show())
  tree.move_window_head()
  println(tree.show())
  tree.move_window_head()
  println(tree.show())
  tree.move_window_head()
  println(tree.show())
  println("==========================================")
  val other_tree = new SuffixTree[Char]
  other_tree.batch_input("vj")
  println(other_tree.show())

  println(tree.show().equals(other_tree.show()))
  println(tree.equals(other_tree))

  def concurrent_demo(): Unit = {
    val system = ActorSystem("SuffixTree")
    val suffix_tree_actor = system.actorOf(Props[SuffixTreeActor], name = "sta")
    val st_input_actor = system.actorOf(Props(new SuffixTreeInputActor(suffix_tree_actor)), name = "ipa")
    val st_query_actor = system.actorOf(Props(new SuffixTreeRegexQuerierActor(suffix_tree_actor)), name = "rqa")

    import system.dispatcher
    val cancelable = system.scheduler.schedule(0 seconds, 5 seconds, st_query_actor, new Tick)
    var exit_flag = false
    while(!exit_flag){
      val command = Console.readLine()
      if(command.startsWith("#p:")){
        val pattern = command slice(3, command.length)
        st_query_actor ! new Pattern(pattern)
      }else if(command == "#exit"){
        exit_flag = true
      }else{
        st_input_actor ! command
      }
    }
    cancelable.cancel()
    system.shutdown()
  }


  def demo(): Unit = {
    val tree = new SuffixTree[Char]
    tree.batch_input("Today is a good day?")
    println(tree.show())
    println("Input regex to query the suffix tree")
    while (true) {
      val input = Console.readLine()
      val p = new Pattern(input)
      println(p.search_pattern(tree).mkString(", "))
    }
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

    val test_cases = Array(
      "this", "what",
      "\\(",
      "(power|a.t) +of +a?",
      "(suf*icient|proximate)+,? and+",
      "go.",
      "\\(...") // the last test case did not pass, but is okay, it is problem of different convention of . operator
    for (t <- test_cases) {
      // scala regex search
      val scala_regex = new Regex(t)
      val sr_match = scala_regex findAllMatchIn target
      val sr_set = new m.HashSet[(BigInt, Int)]()
      sr_match.foreach(m => sr_set.add((m.start, m.end - m.start)))

      // suffix regex search
      val my_regex = new Pattern(t)
      val my_set = new m.HashSet[(BigInt, Int)]()
      val my_result = my_regex.search_pattern(search_suffix_tree)
      my_result.foreach(m => my_set.add(m))

      // comparing the results from above
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
