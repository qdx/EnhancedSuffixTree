package com.qdx.suffixtree.experiments

import com.qdx.suffixtree.suffixtree._


object CorpseTest {

  def recursion_depth(window_size: Int, file_name: String): Double = {
    val input = io.Source.fromURL(getClass.getResource("/" + file_name)).mkString
    val st = new SuffixTree[Char]
    st.window_size = window_size
    input.map(i => {
      st.insert(i)
      st.get_height()
    }).sum.toDouble / input.length.toDouble
  }

}
