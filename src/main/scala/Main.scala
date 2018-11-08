import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}

import scala.annotation.tailrec
import scala.io.Source

object Main extends App {
  val windowSize = 60

  override def main(args: Array[String]): Unit = {
    args.headOption match {
      case Some(filePath) if Files.exists(Paths.get(filePath)) => run(filePath, "output.txt")
      case None => println("Please enter path to existing data input file")
    }
  }

  def run(inFilePath: String, outFilePath: String): Unit = {
    val bw = new BufferedWriter(new FileWriter(new File(outFilePath)))
    bw.write("Time\tValue\tN_O\tRoll_Sum\tMin_Value\tMax_Value\n")

    val observations = Source.fromFile(inFilePath)
      .getLines()
      .flatMap(_.split("\t").toList match {
        case time :: value :: Nil => Some(Observations(time.toLong, value.toDouble))
        case _ => None
      })

    calc(observations, (ob, stats) => {
      bw.write(
        f"${ob.time}\t${ob.value}\t${stats.count}\t${stats.sum}%.5f\t${stats.min}%.5f\t${stats.max}%.5f\n"
      )
    })

    bw.close()
    println(s"See result at: $outFilePath")
  }

  def calc(observations: Iterator[Observations], f: (Observations, Stats) => Unit): Unit = {
    observations.foldLeft(List[Observations]() -> Stats(0, Int.MaxValue, Int.MinValue, 0)) {
      case ((series, stats), ob) =>

        @tailrec
        def calcStats(st: Stats, obs: List[Observations]): (Stats, List[Observations]) = {
          if (obs.isEmpty || ob.time - obs.head.time <= windowSize) {
            val window = obs :+ ob
            val newStats = Stats(
              st.sum + ob.value,
              Math.min(st.min, ob.value),
              Math.max(st.max, ob.value),
              window.length
            )
            (newStats, window)
          }
          else {
            val newStats = Stats(
              st.sum - obs.head.value,
              if (obs.tail.nonEmpty) obs.tail.map(_.value).min else ob.value,
              if (obs.tail.nonEmpty) obs.tail.map(_.value).max else ob.value,
              obs.tail.length - 1
            )
            calcStats(newStats, obs.tail)
          }
        }

        val (newStats, newSeries) = calcStats(stats, series)
        f(ob, newStats)
        (newSeries, newStats)
    }
  }
}