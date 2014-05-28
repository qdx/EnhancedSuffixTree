package com.qdx.demo

import akka.actor.{ActorRef, Actor}
import com.qdx.regex.Pattern

class SuffixTreeRegexQuerierActor(worker: ActorRef) extends Actor {
  def receive = {
    case p: Pattern => worker ! p
    case t: Tick => worker ! new Tick
    case _ =>
  }
}
