package com.raphtory.core.components.orchestration.componentconnector

import akka.actor.Props
import akka.cluster.pubsub.DistributedPubSubMediator
import com.raphtory.core.components.graphbuilder.{GraphBuilder, BuilderExecutor}
import akka.pattern.ask
import com.raphtory.core.components.RaphtoryActor.buildersPerServer
import com.raphtory.core.components.orchestration.raphtoryleader.WatchDog.Message.RequestBuilderId

import scala.concurrent.Future

class BuilderConnector[T](graphBuilder: GraphBuilder[T]) extends ComponentConnector() {

  override def callTheWatchDog(): Future[Any] = {
    log.debug(s"Attempting to retrieve Builder Id from WatchDog.")
    mediator ? DistributedPubSubMediator.Send("/user/WatchDog", RequestBuilderId, localAffinity = false)
  }

  override def giveBirth(assignedId: Int): Unit = {
    log.info(s"Builder Machine $assignedId has come online.")
    val startRange = assignedId*buildersPerServer
    val endRange = startRange+buildersPerServer
    (startRange until endRange).map { i =>
      val tempGraphBuilder = Class.forName(graphBuilder.getClass.getCanonicalName).getConstructor().newInstance().asInstanceOf[GraphBuilder[T]]
      context.system.actorOf(Props(new BuilderExecutor(tempGraphBuilder, i)).withDispatcher("builder-dispatcher"), s"build_$i")
    }.toList

  }
}
