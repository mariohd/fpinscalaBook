import scala.annotation.tailrec

/**
  * Created by MarioDiniz on 01/08/16.
  */
object simpsonRule {
  def oneThirdRule(strips: Int)(min: Double, max: Double, f: Double => Double): Double = {

    val step = Math.abs(max - min) / strips.toDouble

    def k(i:Int): Int = {
      i match {
        case `strips` | 0 => 1
        case _ if (i % 2 == 0) => 2
        case default => 4
      }
    }

    @tailrec
    def loop (x: Double, i: Int, acc: Double = 0): Double = {
      if (x >= max) { acc } else {
        loop(x + step, i + 1, (k(i) * f(x)) + acc)
      }
    }

    loop(min, 0) * (step / 3)
  }

  def threeOfEightRule(strips: Int)(min: Double, max: Double, f: Double => Double): Double = {

    val step = Math.abs(max - min) / strips

    def k(i:Int): Int = {
      i match {
        case `strips` | 0 => 1
        case _ if (i % 3 == 0) => 2
        case default => 3
      }
    }

    @tailrec
    def loop(x: Double, i: Int, acc: Double = 0): Double = {
      if (x >= max) { acc } else {
        loop(x + step, i + 1, (k(i) * f(x)) + acc)
      }
    }

    loop(min, 0) * (3 * step / 8)
  }
}

