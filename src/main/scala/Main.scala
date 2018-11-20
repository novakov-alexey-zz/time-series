import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}

import Config._

import scala.annotation.tailrec
import scala.io.Source

object Config {
  val windowSize = 60
}

object Main extends App {

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
        case time :: value :: Nil => Some(Observation(time.toLong, value.toDouble))
        case _ => None
      })

    val fileWriter: (Observation, WindowStats) => Unit = (ob, stats) => {
      bw.write(f"${ob.time}\t${ob.value}\t${stats.count}\t${stats.sum}%.5f\t${stats.min}%.5f\t${stats.max}%.5f\n")
    }
    calc(observations, fileWriter)

    bw.close()
    println(s"See result at: $outFilePath")
  }

  def calc(observations: Iterator[Observation], f: (Observation, WindowStats) => Unit): Unit = {
    observations.foldLeft(List[Observation]() -> WindowStats(0, Int.MaxValue, Int.MinValue, 0)) {
      case ((series, stats), ob) =>

        @tailrec
        def calcStats(st: WindowStats, obs: List[Observation]): (WindowStats, List[Observation]) = {
          obs match {
            case h :: t if ob.time - obs.head.time > windowSize =>
              val newStats = WindowStats(
                st.sum - h.value,
                if (t.nonEmpty) t.map(_.value).min else ob.value,
                if (t.nonEmpty) t.map(_.value).max else ob.value,
                t.length - 1
              )
              calcStats(newStats, t)
            case _ =>
              val window = obs :+ ob
              val newStats = WindowStats(
                st.sum + ob.value,
                Math.min(st.min, ob.value),
                Math.max(st.max, ob.value),
                window.length
              )
              (newStats, window)
          }
        }

        val (newStats, newSeries) = calcStats(stats, series)
        f(ob, newStats)
        (newSeries, newStats)
    }
  }
}