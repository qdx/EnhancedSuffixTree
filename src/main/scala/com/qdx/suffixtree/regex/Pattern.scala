package com.qdx.suffixtree.regex

import scala.collection.{mutable => m}
import scala.collection.mutable.ArrayBuffer
import com.qdx.debugging.Logger
import com.qdx.suffixtree.suffixtree._

object Pattern {
  // some special characters used to handle inner representation
  val CONCAT = 215.toChar
  val EPSILON = 7.toChar
  val ANY = 6.toChar

  val OPERATOR_PRECEDENCE = new m.HashMap[Char, Int]()
  OPERATOR_PRECEDENCE('(') = 1
  OPERATOR_PRECEDENCE('|') = 2
  OPERATOR_PRECEDENCE(CONCAT) = 3

  OPERATOR_PRECEDENCE('?') = 4
  OPERATOR_PRECEDENCE('*') = 4
  OPERATOR_PRECEDENCE('+') = 4

  OPERATOR_PRECEDENCE('.') = 6
  OPERATOR_PRECEDENCE('\\') = 7


  val LITERAL_SET = new m.HashSet[Char]
  Range('a', 'z').foreach(c => LITERAL_SET += c.toChar)
  Range('A', 'Z').foreach(c => LITERAL_SET += c.toChar)
  LITERAL_SET += '_'
  LITERAL_SET += ','
  LITERAL_SET += '!'
  LITERAL_SET += ' '
  LITERAL_SET += '\n'
  LITERAL_SET += '\r'
  LITERAL_SET += '\''
  LITERAL_SET ++= OPERATOR_PRECEDENCE.keySet

  val UNARY_OPERATOR = Array('?', '*', '+')
  val BINARY_OPERATOR = Array('|')

  private def get_precedence(c: Char): Int = {
    if (OPERATOR_PRECEDENCE.contains(c)) OPERATOR_PRECEDENCE(c)
    else 6
  }
}

class Pattern(p: String) extends Logger {
  log_level = Logger.ERROR
  private val dfa = regex_to_dfa(p)

  // print the FA into a dot language string
  def show(fa: FiniteAutomaton = dfa.get): String = {
    val id_map = m.HashMap[State, Int]()
    val queue = new m.Queue[State]()
    queue.enqueue(fa.start)

    val visited = ArrayBuffer[Int]()
    visited.append(0)

    id_map(fa.start) = 0

    var id_counter = 1

    val sb = new StringBuilder(
      "\ndigraph suffixTree{\n rankdir = LR;\n node [shape=circle, label=\"\", fixedsize=true]\n")

    while (queue.nonEmpty) {
      val s = queue.length

      val add_queue = new m.Queue[State]()

      for (n <- queue) {
        for ((k, v) <- n.to) {
          for (i <- v) {
            id_counter += {
              if (!id_map.contains(i)) {
                id_map(i) = id_counter
                1
              } else 0
            }
            sb.append(id_map(n)).append(" -> ").append(id_map(i)).append(" [label=\"")
            if (k == Pattern.EPSILON) sb.append("emp")
            else if (k == Pattern.ANY) sb.append("any")
            else sb.append(k)
            sb.append("\"];\n")
            if (i.to.size > 0 && !visited.contains(id_map(i))) {
              add_queue.enqueue(i)
              visited.append(id_map(i))
            }
          }
        }
      }
      queue ++= add_queue
      Range(0, s).foreach(i => queue.dequeue())
    }
    for ((k, v) <- id_map) {
      if (k.accept) sb.append(v + " [label=\"" + s"${id_map(k)}" + "\",shape=doublecircle];\n")
      else sb.append(v + " [label=\"" + s"${id_map(k)}" + "\"];\n")
    }
    sb.append("}")
    sb.toString()
  }

  def search_pattern(t: SuffixTree[Char]): ArrayBuffer[(BigInt, Int)] = {
    val long_result = if (dfa.isDefined) {
      val start = new m.HashSet[State]()
      start.add(dfa.get.start)
      search_pattern_routine(start, t, t.root, 0, 0)
    } else {
      new ArrayBuffer[(BigInt, Int)]()
    }
    val short_result = eliminate_suffixes(long_result)
    if(t.repeated.isDefined && t.repeated.get.sequence.length > 0){
      val repeated_suffix_search = search_pattern(t.repeated.get)
      short_result ++= repeated_suffix_search.map(i => (i._1 + t.remainder_index, i._2))
    }
    short_result
  }

  private def eliminate_suffixes(result: ArrayBuffer[(BigInt, Int)]): ArrayBuffer[(BigInt, Int)] = {
    // keep the longest match by eliminating short results
    val e_s = new m.HashMap[BigInt, BigInt]()
    for (i <- result) {
      if (e_s.contains(i._1 + i._2)) {
        if (e_s(i._1 + i._2) > i._1) e_s(i._1 + i._2) = i._1
      } else {
        e_s(i._1 + i._2) = i._1
      }
    }
    val short_result = new ArrayBuffer[(BigInt, Int)]()
    for (i <- e_s) {
      short_result.append((i._2, (i._1 - i._2).toInt))
    }
    short_result
  }

  // recursive search regex pattern on the suffix tree
  private def search_pattern_routine(s: m.HashSet[State],
                                     t: SuffixTree[Char],
                                     n: Node[Char],
                                     l: Int,
                                     previous_accept_length: Int): ArrayBuffer[(BigInt, Int)] = {
    val result = new ArrayBuffer[(BigInt, Int)]()
    if (n.type_ == Node.LEAF_NODE) {
      if (previous_accept_length > 0) result.append((n.search_index_ - t.window_head, l))
    } else {
      for ((k, v) <- n.edges) {
        val label = v.get_label_seq(t.sequence, t.window_head)
        debug("searching label:" + label)
        // ALTERNATE: there should be a better way to unpack the returned results
        val match_str = match_string(label.mkString, s)
        val match_count = match_str._1
        val match_state = match_str._2
        val accept_count = match_str._3
        val previous_accept_state = match_str._4
        debug("matched: " + match_count)
        debug("accepted: " + accept_count)
        debug("previous is:" + previous_accept_state.size)

        // only when match_count == label.length && the recursive search result is
        // not empty we will discard previous accept state
        val recur_search_result =
          if (match_count == label.length) {
            val accept_length = if (accept_count > 0) l + accept_count else previous_accept_length
            val r = search_pattern_routine(match_state, t, v.to, l + match_count, accept_length)
            debug("recur search result size: " + r.size)
            if (r.size > 0) Some(r)
            else None
          }
          else None

        recur_search_result match {
          case Some(r) => result ++= r
          case None =>
            debug("after recur 0 size, previous is:" + previous_accept_state.size)
            if (previous_accept_state.size > 0 || previous_accept_length > 0)
              t.breadth_first_traverse(v.to)
                .filter(n => n.type_ == Node.LEAF_NODE)
                .foreach(leaf => result.append((leaf.search_index_ - t.window_head, l + accept_count)))
        }
      }
    }
    result
  }

  // try to match a sequence of characters based on current states in the DFA
  private def match_string(str: String, s: m.HashSet[State]): (Int, m.HashSet[State], Int, m.HashSet[State]) = {
    var previous_accept_state = new m.HashSet[State]()
    var match_count = 0
    var match_flag = true
    var match_state = new m.HashSet[State]()
    match_state ++= s
    var accept_match_count = 0
    while (match_flag && match_count < str.length) {
      val try_match = match_one_char(str(match_count), match_state)
      if (try_match.size == 0) match_flag = false
      else {
        match_count += 1
        match_state = try_match
        if (match_state.exists(p => p.accept)) {
          previous_accept_state ++= try_match
          accept_match_count = match_count
        }
      }
    }
    (match_count, match_state, accept_match_count, previous_accept_state)
  }

  // try to match one character based on current states in the DFA
  private def match_one_char(c: Char, s: m.HashSet[State]): m.HashSet[State] = {
    val result = new m.HashSet[State]()
    val c_flag = s.exists(p => p.to.contains(c))
    val a_flag = s.exists(p => p.to.contains(Pattern.ANY))
    (c_flag, a_flag) match {
      case (false, false) => Unit
      case (false, true) =>
        s.foreach(s => result ++= s.to(Pattern.ANY))
      case (true, false) =>
        s.foreach(s => result ++= s.to(c))
      case (true, true) =>
        s.foreach(s => result ++= s.to(Pattern.ANY))
        s.foreach(s => result ++= s.to(c))
    }
    result
  }

  def regex_to_dfa(re: String): Option[FiniteAutomaton] = {
    if (re.length() >= 1) {
      val add_concat = add_explicit_concat(re)
      val postfix = infix_to_postfix(add_concat)
      val nfa = postfix_to_nfa(postfix)
      Some(nfa_to_dfa(nfa))
    } else None
  }

  private def add_explicit_concat(re: String): String = {
    val result = new m.StringBuilder()
    var prev = None: Option[Char]
    for (c <- re) {
      val p = prev.getOrElse()
      if (prev.isEmpty
        || p == '('
        || p == '\\'
        || Pattern.BINARY_OPERATOR.contains(p)
        || c == ')'
        || (Pattern.OPERATOR_PRECEDENCE.contains(c) && c != '(' && c != '.' && c != '\\')) {
        result.append(c)
      } else {
        result.append(Pattern.CONCAT + "" + c)
      }
      prev = Some(c)
    }
    result.toString()
  }

  // standard stack to postfix approach
  private def infix_to_postfix(postfix_re: String): String = {
    val stack = new m.Stack[Char]()
    val result = new m.StringBuilder()
    for (c <- postfix_re) {
      c match {
        case '(' => stack.push(c)
        case ')' =>
          while (stack.top != '(') result.append(stack.pop())
          stack.pop()
        case _ =>
          while (!stack.isEmpty && Pattern.get_precedence(stack.top) > Pattern.get_precedence(c)) {
            result.append(stack.pop())
          }
          stack.push(c)
      }
    }
    while (stack.nonEmpty) result.append(stack.pop())

    // ALTERNATE: refactor this into a method
    // the following is to swap the \ and any character after it in the postfix expression
    val swap_escape = result.toString().toCharArray
    var skip = false
    for (i <- Range(0, swap_escape.length - 1)) {
      if (!skip) {
        if (swap_escape(i) == '\\') {
          swap_escape(i) = swap_escape(i + 1)
          swap_escape(i + 1) = '\\'
          skip = true
        }
      } else {
        skip = false
      }
    }
    swap_escape.mkString
  }

  // McMaughton-Yamada-Thompson algorithm which transform postfix regex into nfa
  private def postfix_to_nfa(re: String): FiniteAutomaton = {
    val stack = new m.Stack[FiniteAutomaton]()
    for (i <- Range(0, re.length)) {
      if (i < re.length - 1 && re(i + 1) == '\\') {
        nfa_char(re(i), stack)
      } else {
        re(i) match {
          case '|' => nfa_or(stack)
          case '?' => nfa_question(stack)
          case '+' => nfa_plus(stack)
          case '*' => nfa_star(stack)
          case '.' => nfa_dot(stack)
          case '\\' => Unit
          case Pattern.CONCAT => nfa_concat(stack)
          case _ =>
            if (!Pattern.LITERAL_SET.contains(re(i)))
              error("char not in the literal detected")
            nfa_char(re(i), stack)
        }
      }
    }
    stack.pop()
  }

  // In the following, S is start, E is end, e is epsilon, s_2 is state
  // S---[c]--->E
  private def nfa_char(c: Char, stack: m.Stack[FiniteAutomaton]): Unit = {
    val nfa = new FiniteAutomaton
    nfa.start.add_next(c, nfa.end)
    nfa.end.accept = true
    stack.push(nfa)
  }

  // S---[ANY]--->E
  private def nfa_dot(stack: m.Stack[FiniteAutomaton]): Unit = {
    val nfa = new FiniteAutomaton
    nfa.start.add_next(Pattern.ANY, nfa.end)
    nfa.end.accept = true
    stack.push(nfa)
  }

  // S---e--->nfa1---e---->E
  //  \---e--->nfa2---e-->/
  private def nfa_or(stack: m.Stack[FiniteAutomaton]): Unit = {
    val new_nfa = new FiniteAutomaton
    val nfa1 = stack.pop()
    val nfa2 = stack.pop()
    nfa1.end.accept = false
    nfa2.end.accept = false
    new_nfa.start.add_next(Pattern.EPSILON, nfa1.start)
    new_nfa.start.add_next(Pattern.EPSILON, nfa2.start)
    nfa1.end.add_next(Pattern.EPSILON, new_nfa.end)
    nfa2.end.add_next(Pattern.EPSILON, new_nfa.end)
    new_nfa.end.accept = true
    stack.push(new_nfa)
  }

  // S--->nfa1---e--->nfa2--->E
  private def nfa_concat(stack: m.Stack[FiniteAutomaton]): Unit = {
    val new_nfa = new FiniteAutomaton
    val nfa1 = stack.pop()
    val nfa2 = stack.pop()
    nfa2.end.accept = false
    new_nfa.start = nfa2.start
    new_nfa.end = nfa1.end
    nfa2.end.add_next(Pattern.EPSILON, nfa1.start)
    stack.push(new_nfa)
  }

  // S------e------E
  //  \--->nfa--->/
  private def nfa_question(stack: m.Stack[FiniteAutomaton]): Unit = {
    val nfa = stack.pop()
    nfa.start.add_next(Pattern.EPSILON, nfa.end)
    stack.push(nfa)
  }

  //  /<----e-----\
  // S------e----->E
  //  \--->nfa--->/
  private def nfa_star(stack: m.Stack[FiniteAutomaton]): Unit = {
    val nfa = stack.pop()
    nfa.end.add_next(Pattern.EPSILON, nfa.start)
    nfa.start.add_next(Pattern.EPSILON, nfa.end)
    stack.push(nfa)
  }

  //  /<----e-----\
  // S---->nfa---->E
  private def nfa_plus(stack: m.Stack[FiniteAutomaton]): Unit = {
    val nfa = stack.pop()
    nfa.end.add_next(Pattern.EPSILON, nfa.start)
    stack.push(nfa)
  }

  // subset construction algorithm to transform nfa into dfa, which uses
  // helper method move() and epsilon_closure() below.
  private def nfa_to_dfa(nfa: FiniteAutomaton): FiniteAutomaton = {
    val dfa = new FiniteAutomaton

    // prepare the start state
    val d_to_n_set = new m.HashMap[m.HashSet[State], State]()
    val dfa_start = new m.HashSet[State]()
    // these nfa states form the start state of the dfa
    dfa_start ++= epsilon_closure(Array(nfa.start))
    dfa_start.add(nfa.start)
    d_to_n_set(dfa_start) = new State
    dfa.start = d_to_n_set(dfa_start)

    // a BFS-like traverse on the nfa
    val queue = new m.Queue[m.HashSet[State]]()
    queue.enqueue(dfa_start)
    while (queue.nonEmpty) {
      val l = queue.length
      // states that visisted in this loop that need to be added back to the queue
      val add_queue = new m.Queue[m.HashSet[State]]()
      for (i <- queue) {
        // if there is any state in the nfa states the dfa state represents is an
        // accepting state, the dfa state is an accepting state
        d_to_n_set(i).accept = i.exists(p => p.accept)
        for (s <- i) {
          for ((k, v) <- s.to) {
            if (!d_to_n_set(i).to.contains(k) && k != Pattern.EPSILON) {
              val to_set = move(k, i)
              val epsilon_to_set = epsilon_closure(to_set) ++= to_set
              val hash_epsilon_to_state_set = new m.HashSet[State]()
              hash_epsilon_to_state_set ++= epsilon_to_set
              if (d_to_n_set.contains(hash_epsilon_to_state_set)) {
                d_to_n_set(i).add_next(k, d_to_n_set(hash_epsilon_to_state_set))
              } else {
                val to_state = new State
                d_to_n_set(hash_epsilon_to_state_set) = to_state
                d_to_n_set(i).add_next(k, to_state)
              }
              add_queue.enqueue(hash_epsilon_to_state_set)
            }
          }
        }
      }
      queue ++= add_queue
      Range(0, l).foreach(_ => queue.dequeue())
    }
    dfa
  }

  // subset construction algorithm helper method
  private def move(via: Char, states: Iterable[State]): m.HashSet[State] = {
    val result = new m.HashSet[State]()
    states.filter(s => s.to.contains(via)).foreach(ms => result ++= ms.to(via))
    result
  }

  // subset construction algorithm helper method
  private def epsilon_closure(states: Iterable[State]): ArrayBuffer[State] = {
    val stack = new m.Stack[State]()
    val result = new ArrayBuffer[State]()
    states.foreach(s => stack.push(s))
    while (stack.nonEmpty) {
      val s = stack.pop()
      if (s.to.contains(Pattern.EPSILON)) {
        for (t <- s.to(Pattern.EPSILON)) {
          if (!result.contains(t)) {
            result.append(t)
            stack.push(t)
          }
        }
      }
    }
    result
  }
}
