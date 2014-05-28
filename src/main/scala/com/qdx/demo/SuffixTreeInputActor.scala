package com.qdx.demo

import akka.actor.{ActorRef, Actor}

class SuffixTreeInputActor(worker: ActorRef) extends Actor {
  def receive = {
    case c: Char => worker ! c
    case s: String => worker ! s
    case _ =>
  }

}
