package com.qdx.suffixtree.experiments

object Timing {

  def time[A](f: => A) = {
    val s = System.nanoTime
    f
    (System.nanoTime - s) / 1e6
  }

}
