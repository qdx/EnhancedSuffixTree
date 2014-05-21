package com.qdx.regex

import scala.collection.mutable
import com.qdx.logging.Logger
import scala.collection.mutable.ArrayBuffer

object Pattern {
  val CONCAT = 215.toChar
  val EPSILON = 7.toChar
  val ANY = 6.toChar

  val LITERAL_SET = new mutable.HashSet[Char]
  Range('a', 'z').foreach(c => LITERAL_SET += c.toChar)
  Range('A', 'Z').foreach(c => LITERAL_SET += c.toChar)
  LITERAL_SET += '_'
  LITERAL_SET += '\''

  val OPERATOR_PRECEDENCE = new mutable.HashMap[Char, Int]()
  OPERATOR_PRECEDENCE('(') = 1
  OPERATOR_PRECEDENCE('|') = 2
  OPERATOR_PRECEDENCE(CONCAT) = 3

  OPERATOR_PRECEDENCE('?') = 4
  OPERATOR_PRECEDENCE('*') = 4
  OPERATOR_PRECEDENCE('+') = 4

  OPERATOR_PRECEDENCE('.') = 6
  OPERATOR_PRECEDENCE('\\') = 6

  val UNARY_OPERATOR = Array('?', '*', '+')
  val BINARY_OPERATOR = Array('|')

  private def get_precedence(c: Char): Int = {
    if (OPERATOR_PRECEDENCE.contains(c)) OPERATOR_PRECEDENCE(c)
    else 6
  }
}

class Pattern(p: String) extends Logger {
  log_level = Logger.ERROR
  val dfa = regex_to_dfa(p)

  def regex_to_dfa(re: String): FiniteAutomaton = {
    val postfix = infix_to_postfix(add_explicit_concat(re))
    val nfa = postfix_to_nfa(postfix)
    nfa_to_dfa(nfa)
  }

  private def infix_to_postfix(postfix_re: String): String = {
    val stack = new mutable.Stack[Char]()
    val result = new mutable.StringBuilder()
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
    result.toString()
  }

  private def add_explicit_concat(re: String): String = {
    val result = new mutable.StringBuilder()
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

  private def nfa_to_dfa(nfa: FiniteAutomaton): FiniteAutomaton = {
    println(show(nfa))
    val dfa = new FiniteAutomaton

    // prepare the start state
    val d_to_n_set = new mutable.HashMap[mutable.HashSet[State], State]()
    val dfa_start = new mutable.HashSet[State]()
    dfa_start ++= epsilon_closure(Array(nfa.start))
    dfa_start.add(nfa.start)
    d_to_n_set(dfa_start) = new State
    dfa.start = d_to_n_set(dfa_start)
    assert(d_to_n_set.contains(dfa_start))

    val queue = new mutable.Queue[mutable.HashSet[State]]()
    queue.enqueue(dfa_start)
    while (queue.nonEmpty) {
      val l = queue.length
      val add_queue = new mutable.Queue[mutable.HashSet[State]]()
      for (i <- queue) {
        d_to_n_set(i).accept = i.exists(p => p.accept)
        debug("queue size:" + queue.length)
        for (s <- i) {
          for ((k, v) <- s.to) {
            if (!d_to_n_set(i).to.contains(k) && k != Pattern.EPSILON) {
              debug("to: " + k)
              val to_set = move(k, i)
              val epsilon_to_set = epsilon_closure(to_set) ++= to_set
              val hash_epsilon_to_state_set = new mutable.HashSet[State]()
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
        System.in.read()
      }
      queue ++= add_queue
      Range(0, l).foreach(_ => queue.dequeue())
    }
    dfa
  }

  private def move(via: Char, states: Iterable[State]): ArrayBuffer[State] = {
    val result = new ArrayBuffer[State]()
    for (s <- states) {
      if (s.to.contains(via)) {
        result ++= s.to(via)
      }
    }
    result
  }

  private def epsilon_closure(states: Iterable[State]): ArrayBuffer[State] = {
    val stack = new mutable.Stack[State]()
    val result = new ArrayBuffer[State]()
    for (s <- states) {
      stack.push(s)
    }
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

  private def postfix_to_nfa(re: String): FiniteAutomaton = {
    val stack = new mutable.Stack[FiniteAutomaton]()
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

  private def nfa_char(c: Char, stack: mutable.Stack[FiniteAutomaton]): Unit = {
    val nfa = new FiniteAutomaton
    nfa.start.add_next(c, nfa.end)
    nfa.end.accept = true
    stack.push(nfa)
  }

  private def nfa_dot(stack: mutable.Stack[FiniteAutomaton]): Unit = {
    val nfa = new FiniteAutomaton
    nfa.start.add_next(Pattern.ANY, nfa.end)
    nfa.end.accept = true
    stack.push(nfa)
  }

  private def nfa_or(stack: mutable.Stack[FiniteAutomaton]): Unit = {
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

  private def nfa_concat(stack: mutable.Stack[FiniteAutomaton]): Unit = {
    val new_nfa = new FiniteAutomaton
    val nfa1 = stack.pop()
    val nfa2 = stack.pop()
    nfa2.end.accept = false
    new_nfa.start = nfa2.start
    new_nfa.end = nfa1.end
    nfa2.end.add_next(Pattern.EPSILON, nfa1.start)
    stack.push(new_nfa)
  }

  private def nfa_question(stack: mutable.Stack[FiniteAutomaton]): Unit = {
    val nfa = stack.pop()
    nfa.start.add_next(Pattern.EPSILON, nfa.end)
    stack.push(nfa)
  }

  private def nfa_star(stack: mutable.Stack[FiniteAutomaton]): Unit = {
    val nfa = stack.pop()
    nfa.end.add_next(Pattern.EPSILON, nfa.start)
    nfa.start.add_next(Pattern.EPSILON, nfa.end)
    stack.push(nfa)
  }

  private def nfa_plus(stack: mutable.Stack[FiniteAutomaton]): Unit = {
    val nfa = stack.pop()
    nfa.end.add_next(Pattern.EPSILON, nfa.start)
    stack.push(nfa)
  }

  def show(fa: FiniteAutomaton): String = {
    val id_map = mutable.HashMap[State, Int]()
    val queue = new mutable.Queue[State]()
    queue.enqueue(fa.start)

    val visited = ArrayBuffer[Int]()
    visited.append(0)

    //    val id_map = new mutable.HashMap[State, Int]()
    id_map(fa.start) = 0

    var id_counter = 1

    val sb = new StringBuilder(
      "\ndigraph suffixTree{\n rankdir = LR;\n node [shape=circle, label=\"\", fixedsize=true]\n")

    while (queue.nonEmpty) {
      val s = queue.length

      val add_queue = new mutable.Queue[State]()

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
}
