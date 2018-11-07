import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}

import scala.annotation.tailrec
import scala.io.Source

object Main extends App {

  override def main(args: Array[String]): Unit = {
    args.headOption match {
      case Some(filePath) if Files.exists(Paths.get(filePath)) => run(filePath, "output.txt")
      case None => println("Please enter path to existing data input file")
    }
  }

  private def run(inFilePath: String, outFilePath: String): Unit = {
    val observations = Source.fromFile(inFilePath).getLines()
      .map { l =>
        val row = l.split("\t")
        Observations(row(0).toLong, row(1).toDouble)
      }

    val windowSize = 60
    val bw = new BufferedWriter(new FileWriter(new File(outFilePath)))

    bw.write("Time\tValue\tN_O\tRoll_Sum\tMin_Value\tMax_Value\n")

    observations.foldLeft(List[Observations]() -> Stats(0, Int.MaxValue, Int.MinValue, 0)) {
      case ((series, stats), ob) =>

        @tailrec
        def calcStats(st: Stats, ss: List[Observations]): (Stats, List[Observations]) = {
          if (ss.isEmpty || ob.time - ss.head.time <= windowSize) {
            val window = ss :+ ob
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
              st.sum - ss.head.value,
              if (ss.tail.nonEmpty) ss.tail.map(_.value).min else ob.value,
              if (ss.tail.nonEmpty) ss.tail.map(_.value).max else ob.value,
              ss.tail.length - 1
            )
            calcStats(newStats, ss.tail)
          }
        }

        val (newStats, newSeries) = calcStats(stats, series)
        bw.write(
          f"${ob.time}\t${ob.value}\t${newStats.count}\t${newStats.sum}%.5f\t${newStats.min}%.5f\t${newStats.max}%.5f\n"
        )
        (newSeries, newStats)
    }

    bw.close()
    println(s"See result at: $outFilePath")
  }
}