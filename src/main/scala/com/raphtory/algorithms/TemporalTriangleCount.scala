package com.raphtory.algorithms

import com.raphtory.core.model.algorithm.Analyser

import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.mutable.ParArray

class TemporalTriangleCount(args:Array[String]) extends Analyser[Any](args) {

  override def setup(): Unit =
    view.getVertices().foreach { vertex =>
      val edgeTimes = vertex.getInEdges().map(edge => edge.latestActivity().time)
      val t_max = if(edgeTimes.nonEmpty) edgeTimes.max else -1 //get incoming edgeTimes and then find the most recent edge with respect to timestamp and window
      vertex.getOutEdges(before=t_max).foreach(neighbour => {
        neighbour.send((Array(vertex.ID()),t_max))
      })
    }

  override def analyse(): Unit =
    view.getMessagedVertices().foreach { vertex =>
      val queue = vertex.messageQueue[(Array[Long], Long)]
      queue.foreach(message=> {
        val path = message._1
        val sender = path(path.length-1)
        val t_max = message._2
        val t_min = vertex.getInEdge(sender).get.earliestActivity().time //to include deletions check
        if(path.length<2) { //for step two of the algorithm i.e. the second node in the triangle
          vertex.getOutEdges(before = t_max).foreach(neighbour => {
            neighbour.send((message._1 ++ Array(vertex.ID()), t_max))
          })
        }
        else{ //for the 3rd node in the triangle to see if the final edge exists
          val source = path(0)
          vertex.getOutEdge(source,after=t_min,before=t_max) match {
            case Some(edge) =>
              vertex.appendToState("TrianglePath",(path ++ Array(vertex.ID())).mkString("["," ","]"))
            case None => //No triangle for you
          }
        }
      })
    }

  override def returnResults(): Any =
    view.getVertices()
      .filter(vertex=>
        vertex.containsState("TrianglePath"))
      .flatMap(vertex =>
        vertex.getState[Array[String]]("TrianglePath")).to



  override def extractResults(results: List[Any]): Map[String,Any]  = {
    val endResults = results.asInstanceOf[ArrayBuffer[ParArray[String]]].flatten.toArray
    val toPublish = s"""{triangles:[""" +endResults.map(triangle => triangle+",").fold("")(_+_).dropRight(1)+"]}"
    println(toPublish)
    Map[String,Any]()
  }

  override def defineMaxSteps(): Int = 2
}