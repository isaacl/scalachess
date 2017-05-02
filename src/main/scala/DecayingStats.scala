package chess

sealed trait DecayingStats {
  def mean: Float
  def variance: Float
  def record(value: Float): DecayingStats

  def record(values: Traversable[Float]): DecayingStats =
    values.foldLeft(this) { (s, v) => s record v }

  def stdDev = Math.sqrt(variance).toFloat
}

protected final class DecayingStatsHolder(
    val mean: Float,
    val variance: Float,
    decay: Float
) extends DecayingStats {
  def record(value: Float) = {
    val delta = mean - value

    new DecayingStatsHolder(
      mean = value + decay * delta,
      variance = (1 - decay) * delta * delta + decay * variance,
      decay = decay
    )
  }
}

object DecayingStats {
  def empty(decay: Float = 0.9f) = new DecayingStats {
    def mean = 0f
    def variance = 0f
    def record(value: Float) = new DecayingStatsHolder(
      mean = value,
      variance = 0f,
      decay = decay
    )
  }
}

