package chess

// import org.scalatest._
// import Matchers._

import org.scalactic._
import Tolerance._

import org.specs2.mutable.Specification
import ornicar.scalalib.test.ValidationMatchers

//import scalaz.Validation.FlatMap._

class StatsTest extends Specification with ValidationMatchers {

  def seqMean(elts: Seq[Float]): Float = elts.sum / elts.size

  def seqVariance(elts: Seq[Float]): Double = {
    val mean = seqMean(elts).toDouble
    (elts map { x => Math.pow(x - mean, 2) } sum) / (elts.size - 1)
  }

  val delta = 0.001f

  // def ==~(d: => Double) = beCloseTo(d +/- delta)

  implicit val statsEqual = new Equality[Stats] {
    // def areEqual(a: Stats, b: Any) = true
    def areEqual(a: Stats, b: Any): Boolean = (a, b) match {
      case (StatHolder(aSamp, aMean, aSN), StatHolder(bSamp, bMean, bSN)) => {
        aSamp == bSamp && (aMean === bMean +- delta) && (aSN === bSN +- delta)
      }
      case _ => a == b
    }
  }

  "empty stats" should {
    EmptyStats.variance must_== 0f
    EmptyStats.mean must_== 0f
  }

  "online stats" should {
    // val data = (1 to 100).toArray map { _.toFloat }
    EmptyStats.record(5.0001f) === (new StatHolder record 5)
  }
}
