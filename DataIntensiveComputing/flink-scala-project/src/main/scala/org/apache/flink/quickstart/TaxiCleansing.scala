package org.apache.flink.quickstart

import com.dataartisans.flinktraining.exercises.datastream_java.sources.TaxiRideSource
import com.dataartisans.flinktraining.exercises.datastream_java.utils.GeoUtils
import org.apache.flink.streaming.api.TimeCharacteristic
import org.apache.flink.streaming.api.scala._

/**
  * Created by EinarLogi on 2017-10-05.
  */
/*object TaxiCleansing {
  def main(args: Array[String]): Unit = {
    // get an ExecutionEnvironment
    val env = StreamExecutionEnvironment.getExecutionEnvironment
    // configure event-time processing
    env.setStreamTimeCharacteristic(TimeCharacteristic.EventTime)

    // get the taxi ride data stream
    val rides = env.addSource(
      new TaxiRideSource("/Users/BH/Desktop/KTH/termin3/ID2221 Data-Intensive Computing/lab3/flink-scala-project/nycTaxiRides.gz", 60, 600))

    val nyRides = rides.filter(r: TaxiRide  => )
    val g = 2


  }
}*/
object TaxiCleansing {

  def main(args: Array[String]) {

    // parse parameters
    //val params = ParameterTool.fromArgs(args)
    //val input = params.getRequired("input")

    val maxDelay = 60 // events are out of order by max 60 seconds
    val speed = 600   // events of 10 minutes are served in 1 second

    // set up the execution environment
    val env = StreamExecutionEnvironment.getExecutionEnvironment
    env.setStreamTimeCharacteristic(TimeCharacteristic.EventTime)

    // get the taxi ride data stream
    val rides = env.addSource(new TaxiRideSource("/Users/BH/Desktop/KTH/termin3/ID2221 Data-Intensive Computing/lab3/flink-scala-project/nycTaxiRides.gz", 60, 600))

    val filteredRides = rides
      .filter(r => GeoUtils.isInNYC(r.startLon, r.startLat) && GeoUtils.isInNYC(r.endLon, r.endLat))

    // print the filtered stream
    filteredRides.print()

    // run the cleansing pipeline
    env.execute("Taxi Ride Cleansing")
  }

}