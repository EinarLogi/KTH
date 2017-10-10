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
  * Task 2
  *
  * The airport has a single jumbo bus that can pick up people from the most popular terminal only once per hour and drive them to New York City center.
  * Your task is to extend the previous application in order to generate a stream of the single most popular terminal per hour along with the number of rides
  * and the time of the day as follows:
  *    (TERMINAL_3,43,9)
  *
  * Hint:
  *   You need to run a global/non-parallel window (timeWindowAll) to include counts across terminals.
  *   Then it will be easy to pick the terminal with the maximum count.
  */

object AirportBus extends App {

  val dataPath: String = "/path/to/nycTaxiRides.gz"

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
  val terminalBus = rides
    .map (new GridAirportMatcher) // stream of Terminal objects
    .filter(r => r != Terminal_404) // remove records that were not mapped to a terminal
    .timeWindowAll(Time.hours(1)) // every hour
    .apply{ (window, terminals: Iterable[Terminal], out: Collector[(Terminal, Int, Long)]) =>
      val terminalCounts = terminals.groupBy(k=> k).mapValues(_.size) // return a map of terminal counts of the form Map(terminal_1 => Int, terminal_2 => Int...)
      val maxTerminal = terminalCounts.maxBy(_._2) // select one terminal with the highest Int value, maxBy returns (Terminal, Int)
      out.collect( (maxTerminal._1, maxTerminal._2, window.getEnd) ) // stream of tuples (Terminal, count, time)
    }
    .map(new TimeConverter) // change time representation for each record

  terminalBus.print()
  env.execute()
  
  class GridAirportMatcher extends MapFunction[TaxiRide, Terminal] {

    def map(taxiRide: TaxiRide): Terminal = {
        val gridId: Int = GeoUtils.mapToGridCell(taxiRide.startLon, taxiRide.startLat)
        val terminal = gridToTerminal(gridId)
        taxiRide.isStart match {
          case true => terminal
          case false => Terminal_404
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