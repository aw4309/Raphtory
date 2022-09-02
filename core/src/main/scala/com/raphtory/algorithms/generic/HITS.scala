package com.raphtory.algorithms.generic
import com.raphtory.algorithms.generic.NodeList
import com.raphtory.api.analysis.graphview.GraphPerspective

class HITS(iterateSteps: Int = 100) extends NodeList(Seq("hitshub", "hitsauth")) {

  override def apply(graph: GraphPerspective): graph.Graph =
    graph
      .step { vertex =>
        println("hi")
        val initHub = 1.0; val initAuth = 1.0;
        vertex.setState("hitshub", initHub)
        vertex.setState("hitsauth", initAuth)
        val outDegree = vertex.outDegree
        if (outDegree > 0.0)
          vertex.messageOutNeighbours(List(initHub / outDegree, initAuth / outDegree)) // seq?
      }
      .iterate(
        { vertex =>
          val currHub = vertex.getState[Double]("hitshub")
          val currAuth = vertex.getState[Double]("hitsauth")
          val queue = vertex.messageQueue[List[Double]]

          val newHub = queue.map(l => l(0)).sum
          val newAuth = queue.map(l => l(1)).sum

          println(newHub, newAuth)

          vertex.setState("hitshub", newHub)
          vertex.setState("hitsauth", newAuth)

          val outDegree = vertex.outDegree

          if (outDegree > 0)
            vertex.messageOutNeighbours(List(newHub / outDegree, newAuth / outDegree))

          if (Math.abs(currHub - newHub) < 0.01 & Math.abs(currAuth - newAuth) < 0.01)
            vertex.voteToHalt()
        },
        iterations = iterateSteps,
        executeMessagedOnly = false
      )
}

object HITS {

  def apply(iterateSteps: Int = 100) =
    new HITS(iterateSteps)
}
