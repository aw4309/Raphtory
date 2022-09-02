package com.raphtory.algorithms.generic

import com.raphtory.algorithms.generic.NodeList
import com.raphtory.api.analysis.graphview.GraphPerspective

class HITS(iterateSteps: Int = 100) extends NodeList(Seq("hitshub", "hitsauth")) {

  // normalising on previous max isn't quite right
  override def apply(graph: GraphPerspective): graph.Graph = {
    val initHub = 1.0;
    graph
      .setGlobalState { state =>
        state.newMax[Double]("hubAuthMax", retainState = false)
      }
      .step { (vertex, state) =>
        vertex.setState("hitshub", initHub)

        val outDegree = vertex.outDegree
        if (outDegree > 0.0)
          vertex.messageOutNeighbours(initHub)
      }
      .step { (vertex, state) =>
        val firstHub = vertex.getState[Double]("hitshub") // fix so not double, msg types

        val queue     = vertex.messageQueue[Double]
        val firstAuth = queue.sum / initHub //todo figure out normalising
        vertex.setState("hitsauth", firstAuth)

        state("hubAuthMax") += initHub
        state("hubAuthMax") += firstAuth

        val outDegree = vertex.outDegree
        val inDegree  = vertex.inDegree

        if (outDegree > 0.0)
          vertex.messageOutNeighbours(HubMsg(firstHub)) // seq?
        if (inDegree > 0.0)
          vertex.messageInNeighbours(AuthMsg(firstAuth))
      }
      .iterate(
              { (vertex, state) =>
                val hubAuthMax: Double = state("hubAuthMax").value

                val currHub  = vertex.getState[Double]("hitshub")
                val currAuth = vertex.getState[Double]("hitsauth")
                val queue    = vertex.messageQueue[Message]

                var newAuth: Double = 0.0
                var newHub: Double  = 0.0

                queue.foreach {
                  case HubMsg(value)  =>
                    newHub += (value / hubAuthMax) // normalise

                  case AuthMsg(value) =>
                    newAuth += (value / hubAuthMax)
                }
                //val newAuth = queue.map(l => l(0)).sum / authMax.max(hubMax)
                //val newHub = queue.map(l => l(1)).sum / authMax.max(hubMax)

                state("hubAuthMax") += newHub
                state("hubAuthMax") += newAuth
                println(newHub, newAuth)

                vertex.setState("hitshub", newHub)
                vertex.setState("hitsauth", newAuth)

                val outDegree = vertex.outDegree
                val inDegree  = vertex.inDegree

                if (outDegree > 0.0)
                  vertex.messageOutNeighbours(HubMsg(newHub)) // seq?
                if (inDegree > 0.0)
                  vertex.messageInNeighbours(AuthMsg(newAuth))

                if (Math.abs(currHub - newHub) < 0.00001 & Math.abs(currAuth - newAuth) < 0.00001)
                  vertex.voteToHalt()
              },
              iterations = iterateSteps,
              executeMessagedOnly = false
      )
      .step { (vertex, state) =>
        // normalise the final values
        val hubAuthMax: Double = state("hubAuthMax").value

        val currHub  = vertex.getState[Double]("hitshub")
        val currAuth = vertex.getState[Double]("hitsauth")

        val newAuth = currAuth / hubAuthMax
        val newHub  = currHub / hubAuthMax

        vertex.setState("hitshub", newHub)
        vertex.setState("hitsauth", newAuth)
      }
  }

  sealed trait Message {}
  case class HubMsg(value: Double)  extends Message
  case class AuthMsg(value: Double) extends Message
}

object HITS {

  def apply(iterateSteps: Int = 100) =
    new HITS(iterateSteps)
}
