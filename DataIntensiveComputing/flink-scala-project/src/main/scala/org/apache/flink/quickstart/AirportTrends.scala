import com.dataartisans.flinktraining.exercises.datastream_java.datatypes.TaxiRide
import com.dataartisans.flinktraining.exercises.datastream_java.sources.TaxiRideSource
import com.dataartisans.flinktraining.exercises.datastream_java.utils.GeoUtils
import org.apache.flink.api.common.functions.MapFunction
import org.apache.flink.streaming.api.TimeCharacteristic
import org.apache.flink.streaming.api.scala.{StreamExecutionEnvironment, _}
import org.apache.flink.streaming.api.windowing.time.Time
import org.apache.flink.util.Collector
import java.util.Calendar
import java.util.TimeZone

/**
  * Task 1
  */

object AirportTrends extends App {

  val dataPath: String = "/Users/BH/Desktop/KTH/termin3/ID2221 Data-Intensive Computing/lab3/flink-scala-project/nycTaxiRides.gz"

  sealed trait Terminal{def grid: Int}
  case object Terminal_1 extends Terminal {val grid = 71436}
  case object Terminal_2 extends Terminal {val grid = 71688}
  case object Terminal_3 extends Terminal {val grid = 71191}
  case object Terminal_4 extends Terminal {val grid = 70945}
  case object Terminal_5 extends Terminal {val grid = 70190}
  case object Terminal_6 extends Terminal {val grid = 70686}
  case object Terminal_404 extends Terminal {val grid = -1}

  val terminals : Set[Terminal] = Set(Terminal_1, Terminal_2, Terminal_3, Terminal_4, Terminal_5, Terminal_6)

  def gridToTerminal(gridCell: Int): Terminal = {
    terminals.find(t => t.grid == gridCell) match {
      case Some(terminal) => terminal
      case None => Terminal_404
    }
  }

  val env = StreamExecutionEnvironment.getExecutionEnvironment
  env.setStreamTimeCharacteristic(TimeCharacteristic.EventTime)

  // get the taxi ride data stream - Note: you got to change the path to your local data file
  val rides = env.addSource(new TaxiRideSource(dataPath, 60, 2000))

  /**
    * Write Your application here
    */
  val terminalTaxis = rides
    .map (new GridAirportMatcher) // stream of Terminal objects
    .filter(r => r != Terminal_404) // remove records that were not mapped to a terminal
    .keyBy(k => k) // group by Terminal
    .timeWindow(Time.hours(1)) // every hour
    .apply{ (key: Terminal, window, vals, out: Collector[(Terminal, Int, Long)]) =>
      out.collect( (key, vals.size, window.getEnd) )
    }
    .map(new TimeConverter) // change time representation for each record

  terminalTaxis.print()
  env.execute()

  class GridAirportMatcher extends MapFunction[TaxiRide, Terminal] {

    def map(taxiRide: TaxiRide): Terminal = {
      if(taxiRide.isStart) { // Where taxi ride starts
        val gridId: Int = GeoUtils.mapToGridCell(taxiRide.startLon, taxiRide.startLat)
        val terminal = gridToTerminal(gridId)
        terminal
      }else { // Where taxi ride ends
        val gridId: Int = GeoUtils.mapToGridCell(taxiRide.endLon, taxiRide.endLat)
        val terminal = gridToTerminal(gridId)
        terminal
      }
    }
  }

  class TimeConverter extends MapFunction[(Terminal, Int, Long), (Terminal, Int, Int)] {

    def map(data: (Terminal, Int, Long)): (Terminal, Int, Int) = {

      val calendar = Calendar.getInstance

      calendar.setTimeZone(TimeZone.getTimeZone("America/New_York"))
      calendar.setTimeInMillis(data._3)
      (data._1, data._2, calendar.get(Calendar.HOUR_OF_DAY))
    }
  }

}