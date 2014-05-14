package com.qdx.meta

import scala.util.continuations._

class Generator[A] extends Iterator[A] with (A => Unit @ suspendable) {
  private var a: Option[A] = None
  private var k: Option[Unit => Unit] = None

  def next = {
    val a0 = a.get
    val k0 = k.get
    a = None
    k = None
    k0()
    a0
  }

  def hasNext = k.isDefined

  def apply(a0: A): Unit @ suspendable = {
    a = Some(a0)
    shift { k0: (Unit => Unit) => k = Some(k0) }
  }
}

object Generator {

  def generator[A]( f: (A => Unit @ suspendable) => Unit @ suspendable): Iterator[A] = {
    val g = new Generator[A]
    reset { f(g) }
    g
  }

  trait SuspendableForeach[A]{ def foreach( f: A => Unit @suspendable ): Unit @ suspendable }

  def suspendable[A]( ible: Iterable[A]) = new SuspendableForeach[A] {
    def foreach( f: A => Unit @ suspendable ) : Unit @ suspendable = {
      val i = ible.iterator
      while( i.hasNext ) f(i.next)
    }
  }
}

object Main {
  import Generator._

  def example = generator[String] { yld =>
    yld( "first" )

    for( i <- suspendable(List(1,2,3)); j <- suspendable(List(4,5,6))) {
      yld((i*j).toString)
    }

    yld("last")
  }

  def main(args: Array[String]) {
    for( a <- example ) println(a)
  }
}