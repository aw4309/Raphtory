package com.raphtory.tests

import akka.actor.{ActorSystem, Props}
import ch.qos.logback.classic.Level
import com.raphtory.core.analysis.{AnalysisManager, AnalysisRestApi}
import com.raphtory.core.components.ClusterManagement.{RaphtoryReplicator, WatchDog, WatermarkManager}
import com.raphtory.core.components.Router.GraphBuilder
import kamon.Kamon
import org.slf4j.LoggerFactory

object NaomiTest extends App{

  Kamon.init()

  val root = LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[ch.qos.logback.classic.Logger]
  root.setLevel(Level.ERROR)

  val partitionNumber = 1
  val minimumRouters  = 1

  var Analyser = "com.raphtory.core.analysis.Algorithms.ConnectedComponents"
  Analyser = "com.raphtory.core.analysis.Algorithms.PageRank"

  val start = 1470837600000L
  val end =   31525368897000L

  val jump =    3600000
  var SpoutName = "com.raphtory.examples.lotr.LOTRSpout"
  var routerClassName = "com.raphtory.examples.lotr.LOTRRouter"

  val system = ActorSystem("Single-Node-test")

  system.actorOf(Props(new WatermarkManager(managerCount = 1)),"WatermarkManager")
  system.actorOf(Props(new WatchDog(partitionNumber, minimumRouters)), "WatchDog")
  val graphBuilder = Class.forName(routerClassName).getConstructor().newInstance().asInstanceOf[GraphBuilder[Any]]
  val routerReplicator = RaphtoryReplicator.apply("Router", 1, 1,graphBuilder)
  system.actorOf(Props(routerReplicator), s"Routers")
  system.actorOf(Props(RaphtoryReplicator("Partition Manager", 1,1)), s"PartitionManager")
  system.actorOf(Props(Class.forName(SpoutName)), "Spout")
  val analysisManager = system.actorOf(Props[AnalysisManager], s"AnalysisManager")
  AnalysisRestApi(system)


}