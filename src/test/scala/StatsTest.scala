package chess

import org.specs2.mutable.Specification
import ornicar.scalalib.test.ValidationMatchers

class StatsTest extends Specification with ValidationMatchers {

  def realMean(elts: Seq[Float]): Float = elts.sum / elts.size

  def realVar(elts: Seq[Float]): Float = {
    val mean = realMean(elts).toDouble
    (elts map { x => Math.pow(x - mean, 2) } sum).toFloat / (elts.size - 1)
  }

  def beApprox(comp: Float) = (f: Float) => {
    comp must beCloseTo(f +/- 0.001f * comp)
  }

  def beLike(comp: Stats) = (s: Stats) => {
    s.samples must_== comp.samples
    s.mean must beApprox(comp.mean)
    s.variance must beApprox(comp.variance)
  }

  "empty stats" should {
    "have good defaults" in {
      Stats.empty.variance must_== 0f
      Stats.empty.mean must_== 0f
      Stats.empty.samples must_== 0

      Stats.empty must beLike(StatHolder())
    }

    "convert to StatHolder" in {
      Stats.empty.record(5) must beLike(StatHolder().record(5))

      "with good stats" in {
        Stats.record(5).samples must_== 1
        Stats.record(5).variance must_== 0f
        Stats.record(5).mean must_== 5f
      }
    }

  }

  "large values" should {
    // Tight data w/ large mean. Shuffled for Stats.
    val base = (1 to 100) ++ (1 to 100) ++ (1 to 200)
    val data = base map { _ + 1e5f }
    val shuffledData = base.sortWith(_ % 8 > _ % 8) map { _ + 1e5f }

    val statsN = Stats.empty record shuffledData
    "match actuals" in {
      statsN.mean must beApprox(realMean(data))
      statsN.variance must beApprox(realVar(data))
      statsN.samples must_== 400
    }
    "match concat" in {
      statsN must beLike(Stats.record(data take 1) + Stats.record(data drop 1))
      statsN must beLike(Stats.record(data take 100) + Stats.record(data drop 100))
      statsN must beLike(Stats.record(data take 200) + Stats.record(data drop 200))
    }
  }
}
