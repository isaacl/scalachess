package chess

protected sealed trait StatsT {
  def samples: Int
  def mean: Float
  def variance: Float
  def record(value: Float): StatsT
  def +(o: StatsT): StatsT

  def record(values: Traversable[Float]): StatsT =
    values.foldLeft(this) { (s, v) => s record v }
}

private[chess] final case class StatHolder(
    samples: Int = 0,
    mean: Float = 0f,
    sn: Float = 0f
) extends StatsT {
  def variance = if (samples < 2) 0f else sn / (samples - 1)

  def record(value: Float) = {
    val newSamples = samples + 1
    val delta = value - mean
    val newMean = mean + delta / newSamples
    val newSN = sn + delta * (value - newMean)

    StatHolder(newSamples, newMean, newSN)
  }

  def +(o: StatsT) = o match {
    case StatHolder(oSamples, oMean, oSN) => {
      val total = samples + oSamples
      val combMean = {
        if (samples == oSamples) (mean + oMean) * 0.5f
        else (mean * samples + oMean * oSamples) / total
      }

      val meanDiff = mean - oMean
      val combSn = sn + oSN + (meanDiff * meanDiff * samples * oSamples) / total

      StatHolder(total, combMean, combSn)
    }

    case Stats => this
  }
}

object Stats extends StatsT {
  val samples = 0
  val mean = 0f
  val variance = 0f

  def record(value: Float) = StatHolder(1, value, 0f)
  def +(o: StatsT) = o
}
