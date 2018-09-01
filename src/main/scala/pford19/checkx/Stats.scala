package pford19.checkx

object Stats {

  case class Stats(n: Int, med: Double, avg: Double, min: Double, max: Double, stddev: Double) {
    def pretty: String = f"n=$n, med=$med%.4g, avg=$avg%.4g, min=$min%.4g, max=$max%.4g, stddev=$stddev%.4g"
  }

  def stats(data: Iterable[Double]): Stats = {
    val n = data.size
    val avg = data.sum / n
    val meandiff = data.map(_ - avg)
    val stddev =
      if (n > 1)
        Math.sqrt(meandiff.map(d => d * d).sum / (n - 1))
      else
        0.0
    val (mid1, mid2, midsize) = {
      val n = data.size;
      if (n % 2 == 0) (n / 2 - 1, n / 2 + 1, 2) else (n / 2, n / 2 + 1, 1)
    }
    val med = data.toSeq.sorted.slice(mid1, mid2).sum / midsize
    Stats(n = n, med = med, avg = avg, min = data.min, max = data.max, stddev)
  }

  def intStats(data: Iterable[Int]): Stats = stats(data.map(_.toDouble))

  def distribution[T](data: Iterable[T]): Map[T, Int] = {
    val result = data.groupBy(identity).map { case (key, keyset) => (key, keyset.size) }
    result
  }


}
