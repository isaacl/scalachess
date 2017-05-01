package chess

import org.specs2.mutable.Specification
import ornicar.scalalib.test.ValidationMatchers

class StatsTest extends Specification with ValidationMatchers {

  def realMean(elts: Seq[Float]): Float = elts.sum / elts.size

  def realVar(elts: Seq[Float]): Float = {
    val mean = realMean(elts).toDouble
    (elts map { x => Math.pow(x - mean, 2) } sum).toFloat / (elts.size - 1)
  }

  def beLike(comp: StatsT) = (s: StatsT) => {
    s.samples must_== comp.samples
    s.mean must beCloseTo(comp.mean +/- 0.001f * s.mean)
    s.variance must beCloseTo(comp.variance +/- 0.001f * s.variance)
  }

  "empty stats" should {
    "have good defaults" in {
      Stats.variance must_== 0f
      Stats.mean must_== 0f
      Stats.samples must_== 0

    }
    "convert to stats" in {
      Stats.record(5) must beLike(StatHolder().record(5))
      Stats.record(0) must beLike(StatHolder().record(0))
    }
  }

  "online stats" should {
    val data = ((1 to 200) ++ (1 to 300)).toArray map { _.toFloat }
    val statsN = Stats record data.sortWith(_ % 5 > _ % 3) // Shuffle data
    "match stub" in {
      statsN.mean must (beCloseTo(realMean(data) +/- 0.01f))
      statsN.variance must (beCloseTo(realVar(data) +/- 0.1f))
      statsN.samples must_== 500
    }
    "match concat" in {
      statsN must beLike(Stats.record(data take 5) + Stats.record(data drop 5))
      statsN must beLike(Stats.record(data take 100) + Stats.record(data drop 100))
      statsN must beLike(Stats.record(data take 250) + Stats.record(data drop 250))
    }
  }
}
