import com.qdx.suffixtree.SuffixTree
import com.qdx.suffixtree.Node
import com.qdx.logging.Logger
import org.scalacheck.Prop.forAll
import org.scalacheck._

object SuffixTreeTest extends Properties("Suffix Tree Properties") {
  val alphabet = Gen.choose('a', 'z')
  val aTozString = for{
    size <- Gen.choose(10, 10000)
    s <- Gen.listOfN(size, alphabet)
  } yield s.mkString

  property("numOfLeafNodes") = forAll(aTozString){ s: String =>
    val stree = new SuffixTree[Char]
    stree.log_level = Logger.ERROR
    stree.batch_input(s)
    stree.insert('#')
    var leaf_counter = 0
    for(n <- stree.breadth_first_traverse())
      if(n.type_ == Node.LEAF_NODE)
        leaf_counter += 1
    leaf_counter == s.length + 1 && leaf_counter == stree.remainder_index
  }

  property("leaveReferences") = forAll(aTozString){ s: String =>
    val stree = new SuffixTree[Char]
    stree.log_level = Logger.ERROR
    stree.batch_input(s)
    stree.insert('#')
    stree.leaves.zipWithIndex.forall(i => i._1.isEmpty || i._1.get.search_index_ == i._2)
  }
}