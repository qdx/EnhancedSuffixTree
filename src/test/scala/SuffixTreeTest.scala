import com.qdx.suffixtree.SuffixTree
import com.qdx.suffixtree.Node
import com.qdx.logging.Logger
import org.scalacheck.Prop.forAll
import org.scalacheck._

object SuffixTreeTest extends Properties("Suffix Tree Properties") {
  val alphabet = Gen.choose('a', 'z')
  val aTozString = for {
    size <- Gen.choose(10, 100)
    s <- Gen.listOfN(size, alphabet)
  } yield s.mkString

  property("numOfLeafNodes") = forAll(aTozString) { s: String =>
    val stree = new SuffixTree[Char]
    stree.log_level = Logger.ERROR
    stree.batch_input(s)
    stree.insert('#')
    var leaf_counter = 0
    for (n <- stree.breadth_first_traverse())
      if (n.type_ == Node.LEAF_NODE)
        leaf_counter += 1
    leaf_counter == s.length + 1 && leaf_counter == stree.remainder_index
  }

  property("leafNodeReferences") = forAll(aTozString) { s: String =>
    val stree = new SuffixTree[Char]
    stree.log_level = Logger.ERROR
    stree.batch_input(s)
    stree.insert('#')
    stree.leaves.zipWithIndex.forall(i => i._1.isEmpty || i._1.get.search_index_ == i._2 + stree.window_head)
  }

  property("doublyLinkedTree") = forAll(aTozString) { s: String =>
    val stree = new SuffixTree[Char]
    stree.log_level = Logger.ERROR
    stree.batch_input(s)
    stree.insert('#')
    var loop_flag = true
    var loop_index = 0
    val nodes = stree.breadth_first_traverse()
    while (loop_flag && loop_index < nodes.length) {
      val n = nodes(loop_index)
      if (n.type_ != Node.ROOT) {
        loop_flag = n.from_edge.isDefined && n.from_edge.get.to.equals(n)
      }
      loop_index += 1
    }
    loop_flag && loop_index == nodes.length
  }

  property("equalsWorkCorrectly") = forAll(aTozString) { s: String =>
    val st1 = new SuffixTree[Char]
    val st2 = new SuffixTree[Char]
    st1.batch_input(s)
    st2.batch_input(s)
    st1.equals(st2)
  }

  property("removeHead") = forAll(aTozString) { s: String =>
    val st = new SuffixTree[Char]
    st.batch_input(s)
    Range(0, s.length - 1).forall(i => {
      val st_compare = new SuffixTree[Char]
      st_compare.batch_input(s slice(i, s.length))
      val result = st.equals(st_compare)
      st.move_window_head()
      result
    })
  }
}