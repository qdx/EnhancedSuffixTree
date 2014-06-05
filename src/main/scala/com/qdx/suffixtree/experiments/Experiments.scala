package com.qdx.suffixtree.experiments

import com.qdx.suffixtree.suffixtree.SuffixTree
import objectexplorer.ObjectGraphMeasurer
import scala.collection.mutable.ArrayBuffer
import com.qdx.suffixtree.regex.Pattern

object Experiments {

  def execute_all_experiments(): Unit = {

    different_pattern_match("summaTheologica.txt")
    //    time_to_BFS()
    //    val number = "[0|1|2|3|4|5|6|7|8|9]+"
    //    denews_pattern_match(3000, number)
    //    println(time_of_insert())
    //    println("start executing all experiments!")
    //    println("running recursion depth analysis on denews dataset")
    //    denews_dataset()
    //    println()

    //    println("running pattern match time experiments ")
    //    val number = "[0|1|2|3|4|5|6|7|8|9]+"
    //    val false_pattern = "thiswillnevernappearimsure"
    //    println("\trunning number pattern ")
    //    pattern_match("howto", 1000000, 1000, number)
    //    println("\trunning false pattern ")
    //    pattern_match("howto", 1000000, 1000, false_pattern)
    //    println()
    //
    //    println("running delete head experiment")
    //    delete_head("howto", 1000000, 1000)
    //    println()

    //    println("running sliding window experiment")
    //    sliding_window("howto", 1000000, 1000, 10000)
    //    println()

    //    println("running time to build tree experiment")
    //    time_to_build_tree("howto", 1000000, 1000)
    //    println()

  }

  def time_to_BFS(): Unit = {
    val input = io.Source.fromURL(getClass.getResource("/howto"))("latin1").mkString slice(0, 10000)
    val sb = new StringBuilder()
    sb.append("{")

    0 until 10000 by 100 foreach (d => {
      val st = new SuffixTree[Char]
      st.batch_input(input slice(0, d))
      val time = Timing.time(st.breadth_first_traverse(st.root))
      sb.append(s"{$d, $time},")
    })

    sb.append("}")
    WriteResult.write_to("bfs_time.txt", sb.toString(), "time to bfs suffix tree:")

  }

  def denews_pattern_match(length: Int, pattern: String): Unit = {
    val fnames = io.Source.fromURL(getClass.getResource("/fnames.txt")).getLines()
    val sb = new StringBuilder()
    val times = new ArrayBuffer[(Int, Double)]()
    sb.append("{")
    for (f <- fnames) {
      println(s"/denews/${f.trim}.txt")
      val input = io.Source.fromURL(getClass.getResource(s"/denews/${f.trim}.txt"))("latin1").mkString
      val st = new SuffixTree[Char]
      st.batch_input(input slice(0, length))
      val p = new Pattern(pattern)
      var result = None: Option[ArrayBuffer[(BigInt, Int)]]
      val time = Timing.time({
        result = Some(p.search_pattern(st))
      })
      val size = result.get.size
      times.append((size, time))
    }
    times.sorted.foreach(t => sb.append(s"{${t._1}, ${t._2} },"))
    sb.append("}")
    WriteResult.write_to("denews_pattern_match.txt", sb.toString(), "matching fixed length input, result size X time:")
  }

  // build one suffix tree for each of these input file, for each suffix tree, during the
  // process of building it, get the average recursion depth, and the max depth.
  // then for all these files, calculate the average recursion depth, max depth, standard deviation
  // So we have data:
  // 1. average recursion depth
  // 2. max recursion depth
  // 3. standard deviation of recursion depth among these input
  def denews_dataset(): Unit = {
    val fnames = io.Source.fromURL(getClass.getResource("/fnames.txt")).getLines()
    val avg = new ArrayBuffer[Double]()
    val max = new ArrayBuffer[Int]()
    for (f <- fnames) {
      println(s"/denews/${f.trim}.txt")
      val depth = recursion_depth(s"/denews/${f.trim}.txt")
      avg.append(depth._1)
      max.append(depth._2)
    }
    val sb = new StringBuilder()
    val mean_avg = avg.sum / avg.length.toDouble
    val stddev = math.sqrt(avg.map(d => math.pow(d - mean_avg, 2)).sum / avg.length.toDouble)
    sb.append(s"average recursion depth is:$mean_avg\n")
    sb.append(s"recursion depth standard deviation:$stddev\n")
    sb.append(s"average max depth:${max.sum / max.length.toDouble}\n")

    WriteResult.write_to("denews_result.txt", sb.toString(), "Statics about recursive suffix tree depth:")
  }

  def recursion_depth(input_file: String): (Double, Int) = {
    val input = io.Source.fromURL(getClass.getResource(input_file))("latin1").mkString
    val st = new SuffixTree[Char]
    var max_depth = 0
    val avg_depth = input.map(i => {
      st.insert(i)
      val depth = st.get_height()
      if (depth > max_depth) max_depth = depth
      depth
    }).sum.toDouble / input.length.toDouble
    (avg_depth, max_depth)
  }

  // feed different length of input, from 1e4 to 1e6 with interval 1e4
  // search the same pattern on them, output two groups of data:
  // 1. {input length, time to search}
  // 2. {result size, time to search}
  def pattern_match(input_file: String, input_length: Int = 1000000, interval: Int = 10000, pattern: String): Unit = {
    val input = io.Source.fromURL(getClass.getResource("/" + input_file))("latin1").mkString slice(0, input_length)
    val p = new Pattern(pattern)
    val sb1 = new StringBuilder()
    val sb2 = new StringBuilder()
    sb1.append("{")
    sb2.append("{")
    0 until input_length by interval foreach (i => {
      val st = new SuffixTree[Char]
      st.batch_input(input slice(0, i))
      var result = None: Option[ArrayBuffer[(BigInt, Int)]]
      val time = Timing.time({
        result = Some(p.search_pattern(st))
      })
      sb1.append(s"{$i, $time}, ")
      sb2.append(s"{${result.get.length}, $time}, ")
    })
    sb1.append("}")
    sb2.append("}")
    WriteResult.write_to("pattern_match.txt", sb1.toString(), "input length X time to search")
    WriteResult.write_to("pattern_match.txt", sb2.toString(), "match result size X time to search")
  }

  def different_pattern_match(input_file: String, input_length: Int = 100000): Unit = {
    val input = io.Source.fromURL(getClass.getResource("/" + input_file))("latin1").mkString slice(0, input_length)
    val st = new SuffixTree[Char]
    st.batch_input(input)
    val sb = new StringBuilder()
    val times = new ArrayBuffer[(Int, Double)]()
    sb.append("{")
    val cs = Range('a', 'z') ++ Range('A', 'Z')
    cs.foreach(d => {
      cs.foreach(d2 => {
        val pstr = d.toChar.toString + d2.toChar.toString
        println(pstr)
        val p = new Pattern(pstr)
        var result = None: Option[ArrayBuffer[(BigInt, Int)]]
        val time = Timing.time({
          result = Some(p.search_pattern(st))
        })
        times.append((result.get.size, time))
      })
    })
    times.sorted.foreach(t => sb.append(s"{${t._1}, ${t._2}}, "))
    sb.append("}")
    WriteResult.write_to("fixed_length_pattern_match.txt", sb.toString(), "fixed input length, query different pattern:")

  }

  // feed a long input, then measure the time to delete the head of the input, until the whole
  // tree is deleted, draw one graph:
  // 1. {input length, time to delete the head}
  def delete_head(input_file: String, input_length: Int = 1000000, interval: Int = 100): Unit = {
    val input = io.Source.fromURL(getClass.getResource("/" + input_file))("latin1").mkString slice(0, input_length)
    val sb = new StringBuilder()
    sb.append("{")
    val st = new SuffixTree[Char]
    st.batch_input(input)
    var counter = 0
    0 until input_length foreach (i => {
      if (counter % interval == 0) {
        val time = Timing.time(st.delete_head())
        sb.append(s"{${input_length - i}, $time}, ")
      } else {
        st.delete_head()
      }
      counter += 1
    })
    sb.append("}")
    WriteResult.write_to("delete_head.txt", sb.toString(), "sequence length X time to delete head:")
  }

  // set different window size, 1e3 to 1e5 with interval 1e3 on input length 1e6,
  // see how long it take to slide through a long input
  // draw one graph:
  // 1. {window size, time to slide through}
  def sliding_window(input_file: String, input_length: Int = 1000000, interval: Int = 1000, slide_length: Int = 20000): Unit = {
    val input = io.Source.fromURL(getClass.getResource("/" + input_file))("latin1").mkString slice(0, input_length)
    val sb = new StringBuilder()
    sb.append("{")
    interval until input_length - slide_length by interval foreach (i => {
      val st = new SuffixTree[Char]
      st.window_size = i
      input slice(0, i) foreach (c => st.insert(c))
      println(s"st length:${st.sequence.length}, window size: $i")
      assert(st.sequence.length == i)
      val current_input = input slice(i, i + slide_length)
      val time = Timing.time(current_input.foreach(c => st.insert(c)))
      sb.append(s"{$i, $time}, ")
    })
    sb.append("}")
    WriteResult.write_to("sliding_window.txt", sb.toString(), "sliding window size X time to slide through:")
  }

  // test build time on input length from 1e3 to 1e6 with interval 1e3
  def time_to_build_tree(input_file: String = "howto", input_length: Int = 1000000, interval: Int = 1000): Unit = {
    val target = io.Source.fromURL(getClass.getResource(s"/$input_file"))("latin1").mkString
    val result = new StringBuilder()
    result.append("{")
    0 until input_length by interval foreach (i => {
      val input = target.slice(0, i) + "~"
      val st = new SuffixTree[Char]
      val t_measure = Timing.time(input.foreach(c => st.insert(c)))
      result.append(s"{$i, $t_measure},")
      println(s"testing input length: $i")
    })
    result.append("}")
    WriteResult.write_to("time_to_build.txt", result.toString(), "time to build suffix tree:")
  }

  // measure data structure foot prints, in number of objects, references and primitives
  def space_usage(input_file: String, length: Int = 1000000, interval: Int = 1000, write_to: String): Unit = {
    val st = new SuffixTree[Char]
    val sbs = new ArrayBuffer[StringBuilder]()
    Range(0, 4).foreach(_ => sbs.append(new StringBuilder))
    sbs.foreach(sb => sb.append("{"))
    val target = io.Source.fromURL(getClass.getResource(s"/$input_file"))("latin1")
    val input = target.mkString slice(0, length)
    var count = 0
    for (c <- input) {
      st.insert(c)
      count += 1
      if (count % interval == 0) {
        val measure = ObjectGraphMeasurer.measure(st)
        sbs(0).append(s"{$count, ${measure.getObjects}}, ")
        sbs(1).append(s"{$count, ${measure.getReferences}}, ")
        sbs(2).append(s"{$count, ${measure.getPrimitives.count(Integer.TYPE)}}, ")
        sbs(3).append(s"{$count, ${measure.getPrimitives.count(Character.TYPE)}}, ")
      }
    }
    sbs.foreach(sb => sb.append("}"))
    WriteResult.write_to(write_to, sbs(0).toString(), "number of objects:")
    WriteResult.write_to(write_to, sbs(1).toString(), "number of references:")
    WriteResult.write_to(write_to, sbs(2).toString(), "number of int:")
    WriteResult.write_to(write_to, sbs(3).toString(), "number of char:")
  }

  // measure the time to insert into the suffix tree, the output should be:
  // 1. size of current tree X time to insert
  // the returned value is average time of the insert
  def time_of_insert(input_file: String = "howto", input_length: Int = 1000000): (Double, Double) = {
    val input = io.Source.fromURL(getClass.getResource(s"/$input_file"))("latin1").mkString slice(0, input_length)
    val result = new StringBuilder()
    result.append("{")
    val st = new SuffixTree[Char]
    val times = new ArrayBuffer[Double]()
    0 until input.length foreach (i => {
      val c = input(i)
      val t_measure = Timing.time(st.insert(c))
      if (i % 1000 == 0) {
        result.append(s"{$i, $t_measure},")
        //        println(s"testing input length: $i")
      }
      times.append(t_measure)
    })
    result.append("}")
    //    WriteResult.write_to("time_to_insert.txt", result.toString(), "time to insert suffix tree:")
    val mean_avg = times.sum / times.length
    val stddev = math.sqrt(times.map(d => math.pow(d - mean_avg, 2)).sum / times.length.toDouble)
    (mean_avg, stddev)
  }
}
