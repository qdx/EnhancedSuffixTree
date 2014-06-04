package com.qdx.suffixtree.experiments

import com.qdx.suffixtree.suffixtree._
import objectexplorer.ObjectGraphMeasurer


object Experiments {

  def reuters_dataset(): Unit = {
    val fnames = io.Source.fromURL(getClass.getResource("/fnames.txt")).getLines()
    for (f <- fnames) {
      val time = Timing.time({
        println(f)
        val input = io.Source.fromURL(getClass.getResource(s"/denews/${f.trim}.txt"))("latin1").mkString
        val st = new SuffixTree[Char]
        st.batch_input(input)
      })
      println(time)
    }

  }

  def recursion_depth(file_name: String): Double = {
    val input = io.Source.fromURL(getClass.getResource("/" + file_name)).mkString
    val st = new SuffixTree[Char]
    input.map(i => {
      st.insert(i)
      st.get_height()
    }).sum.toDouble / input.length.toDouble
  }

  def time_to_build_tree(interval: Int): Unit = {
    val target = io.Source.fromURL(getClass.getResource("/summaTheologica.txt")).mkString
    val result = new StringBuilder()
    result.append("{")
    for (i <- Range(1, 1000)) {
      val input = target.slice(0, i * interval) + "~"
      val st = new SuffixTree[Char]
      val t_measure = Timing.time({
        for (c <- input) {
          st.insert(c)
        }
      })
      result.append(s"{${i * interval}, $t_measure},")
      println(s"testing input length: ${i * interval}")
    }
    result.append("}")
    WriteResult.write_to("time_to_build.txt", result.toString(), "time to build suffix tree:")
  }

  def space_usage(): Unit = {
    val st = new SuffixTree[Char]
    val sb1 = new StringBuilder()
    val sb2 = new StringBuilder()
    sb1.append("{")
    sb2.append("{")
    val target = io.Source.fromURL(getClass.getResource("/small.txt"))("latin1")
    var count = 0
    for (c <- target) {
      st.insert(c)
      count += 1
      if (count % 1000 == 0) {
        sb1.append(s"{$count, ${ObjectGraphMeasurer.measure(st).getObjects}}, ")
        sb2.append(s"{$count, ${ObjectGraphMeasurer.measure(st).getReferences}}, ")
      }
    }
    sb1.append("}")
    sb2.append("}")
    WriteResult.write_to("space.txt", sb1.toString(), "number of objects:")
    WriteResult.write_to("space.txt", sb2.toString(), "number of references:")
  }
}
