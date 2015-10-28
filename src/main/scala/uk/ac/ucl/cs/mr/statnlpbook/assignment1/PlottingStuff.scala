import com.quantifind.charts.Highcharts._
import org.sameersingh.scalaplot.gnuplot.GnuplotPlotter
import org.sameersingh.scalaplot.{BarChart, BarData, MemBarSeries}

/**
 * Created by skas on 10/20/15.
 */
object PlottingStuff {
  def main(args: Array[String]) {
    val hashmap_count_bar = new collection.mutable.HashMap[Int, Int]
    var curr_count = 0
    var can_count:Boolean = false
    for(i <- 0 until train.size) {
      if (train(i) == "[BAR]") {
        if (curr_count != 0) {
          hashmap_count_bar(curr_count) = hashmap_count_bar.getOrElse(curr_count, 0) + 1
        }
        can_count = true
        curr_count = 0
      } else if (train(i) == "[/BAR]") {
        can_count = false
      }
      else if (can_count) {
        curr_count += 1
      }
    }
    val x = hashmap_count_bar.toList.sortBy( i => i._1).map(i => i._1)
    val y = hashmap_count_bar.toList.sortBy( i => i._1).map(i => i._2.toDouble)



    val series_b = new MemBarSeries(y, "SOME ?")
    val data_b = new BarData(x.map("" + _), Seq(series_b))
    val chart_b = new BarChart("Powers!", data_b)

    val plotter = new GnuplotPlotter(chart_b)
    plotter.svg("../", "SomeGraph")
  }
}
