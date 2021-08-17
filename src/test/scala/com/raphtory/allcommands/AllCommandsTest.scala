package com.raphtory.allcommands

import com.raphtory.RaphtoryComponent
import org.scalatest.FunSuite
import akka.pattern.ask
import akka.util.Timeout
import com.raphtory.algorithms.StateTest
import com.raphtory.core.actors.analysismanager.AnalysisRestApi.message.{RangeAnalysisRequest, TestAnalysisRequest}
import com.raphtory.core.actors.orchestration.clustermanager.WatermarkManager.Message.{WatermarkTime, WhatsTheTime}
import com.raphtory.core.analysis.api.Analyser
import com.raphtory.serialisers.DefaultSerialiser

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

class AllCommandsTest extends FunSuite {
  //SET
  val partitionCount =4
  val routerCount =3
  val seedNode = new RaphtoryComponent("seedNode",partitionCount,routerCount,1600)
  val analysisManager = new RaphtoryComponent("analysisManager",partitionCount,routerCount,1602)
  val spout= new RaphtoryComponent("spout",partitionCount,routerCount,1603,"com.raphtory.spouts.FileSpout")
  val router1 = new RaphtoryComponent("router",partitionCount,routerCount,1604,"com.raphtory.allcommands.AllCommandsBuilder")
  val router2 = new RaphtoryComponent("router",partitionCount,routerCount,1605,"com.raphtory.allcommands.AllCommandsBuilder")
  val router3 = new RaphtoryComponent("router",partitionCount,routerCount,1606,"com.raphtory.allcommands.AllCommandsBuilder")
  val pm1 = new RaphtoryComponent("partitionManager",partitionCount,routerCount,1614)
  val pm2 = new RaphtoryComponent("partitionManager",partitionCount,routerCount,1615)
  val pm3 = new RaphtoryComponent("partitionManager",partitionCount,routerCount,1616)
  val pm4 = new RaphtoryComponent("partitionManager",partitionCount,routerCount,1617)

  test("Warmup and Ingestion Test") {
      implicit val timeout: Timeout = 20.second
      try {
        var currentTimestamp = 0L
        Thread.sleep(60000) //Wait the initial watermarker warm up time
        for (i <- 1 to 6){
          Thread.sleep(10000)
          val future = seedNode.getWatermarker.get ? WhatsTheTime
          currentTimestamp = Await.result(future, timeout.duration).asInstanceOf[WatermarkTime].time
        }
        assert(currentTimestamp==299868) //all data is ingested and the minimum watermark is set to the last line in the data
    } catch {
      case _: java.util.concurrent.TimeoutException => assert(false)
    }
  }

  test("Graph State Test"){
    val stateTest:Analyser[Any] = new StateTest(Array())
    val serialiser = new DefaultSerialiser()
    analysisManager.getAnalysisManager.get !
      TestAnalysisRequest(stateTest,serialiser,1,290001,10000, List(1000,10000,100000,1000000),Array(),"")

  }
}
