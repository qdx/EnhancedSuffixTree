package com.qdx.suffixtree.demo

import scala.util.matching.Regex
import scala.collection.{mutable => m}
import scala.concurrent.duration._
import scala.collection.mutable.ArrayBuffer

import akka.actor.{Props, ActorSystem}

import com.qdx.suffixtree.suffixtree.{SuffixTreeActor, SuffixTree, Node}
import com.qdx.debugging.Logger
import com.qdx.suffixtree.regex.Pattern
import com.qdx.suffixtree.experiments._

import com.github.jabbalaci.graphviz._
import java.io.{PrintWriter, FileWriter, File}
import java.awt.Desktop

object MainEntry extends App {
  //TODO 1: consider enabling the tree to be configured into both single linked or double linked
  //TODO 2: implement the data structure that can enable log time delete
  val demo_file_path = "d:/suffixtree_demo/"
  /*
  val gv = new GraphViz()
  gv.add(gv.start_graph())
  val st = new SuffixTree[Char]
  st.batch_input("asdf")
  gv.add(st.show())
  gv.add(gv.end_graph())
  println(gv.getDotSource)
  gv.increaseDpi();   // 106 dpi
  val gtype= "gif"
  val rtype = "dot"
  val img_path = demo_file_path + "out." + gtype
  val out = new File(img_path)    // Windows
  gv.writeGraphToFile( gv.getGraph(st.show(), gtype, rtype), out )
  val f = new File(img_path)
  val dt = Desktop.getDesktop
  dt.open(f)
  System.out.println("Done.")
  */
  concurrent_demo()
//  val p = new Pattern("\\(...")

//  val st = new SuffixTree[Char]
//  st.batch_input("abacadd")
//  st.delete_head()
//  println(st.show(label_as_item = false))

//  Experiments.execute_all_experiments()
//  regex_search_test()
//  Experiments.space_usage("howto", write_to = "space_usage.txt")

  def recursive_pattern_search_test(): Unit = {
    val str = "this is a pattern seen before, a pattern"
    val st = new SuffixTree[Char]
    st.batch_input(str)
    val pattern = new Pattern("pat+")
    println(pattern.search_pattern(st).mkString(","))
  }

  def sliding_pattern_search(): Unit = {
    val str = "Today is a good day and I will finish the recursive suffix tree!!!!"
    val st = new SuffixTree[Char]
    st.window_size = 6
    st.slide_size = 3
    val pattern = new Pattern("(a|t)(n|h)")
    for (i <- str) {
      st.insert_wrapper(i)
      println("tree height is:" + st.get_height() + "; " + st.sequence.mkString + st.window_head + pattern.search_pattern(st).mkString(","))
    }
  }

  def sliding_test(s: String): Boolean = {
    val window_size = s.length / 2
    val sliding_tree = new SuffixTree[Char]
    val test_buffer = new ArrayBuffer[Char]
    sliding_tree.window_size = window_size
    Range(0, s.length).forall(i => {
      val st = new SuffixTree[Char]
      val begin = if (i >= window_size) i + 1 - window_size else 0
      val s1 = s slice(begin, i + 1)
      test_buffer.append(s(i))
      while (test_buffer.length > window_size) test_buffer.remove(0)
      assert(s1.equals(test_buffer.mkString))
      st.batch_input(s slice(begin, i + 1))
      sliding_tree.insert_wrapper(s(i))
      println(s"sliding: ${sliding_tree.sequence.mkString}, st:${st.sequence.mkString}")
      assert(sliding_tree.sequence.equals(st.sequence))
      val result = st.equals(sliding_tree)
      println(sliding_tree.show(label_as_item = false))
      println(st.show(label_as_item = false))
      result
    })
  }


  def concurrent_demo(): Unit = {
    val system = ActorSystem("SuffixTree")
    val suffix_tree_actor = system.actorOf(Props[SuffixTreeActor], name = "sta")
    val st_input_actor = system.actorOf(Props(new SuffixTreeInputActor(suffix_tree_actor)), name = "ipa")
    val st_query_actor = system.actorOf(Props(new SuffixTreeRegexQuerierActor(suffix_tree_actor)), name = "rqa")

    import system.dispatcher
    val cancelable = system.scheduler.schedule(0 seconds, 5 seconds, st_query_actor, new Tick)
    var exit_flag = false
    while (!exit_flag) {
      val command = Console.readLine()
      if (command.startsWith("#p:")) {
        val pattern = command slice(3, command.length)
        st_query_actor ! new Pattern(pattern)
        val fw = new FileWriter(demo_file_path + "patterns.txt", true)
        try {
          fw.write(pattern + "\n")
        }
        finally {
          fw.close()
        }
      } else if (command == "#exit") {
        exit_flag = true
      } else {
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
    val target = io.Source.fromURL(getClass.getResource("/small.txt")).mkString + "~"
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
        /* output test result to findout where is wrong*/
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
      test.insert_wrapper(i)
    }
    test.insert_wrapper(terminal)
    var leaf_c = 0
    for (n <- test.breadth_first_traverse()) {
      if (n.type_ == Node.LEAF_NODE) {
        leaf_c += 1
      }
    }
    if (leaf_c == counter + 1) true else false
  }


}
