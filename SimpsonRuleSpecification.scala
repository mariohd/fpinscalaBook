import org.scalacheck.Prop._
import org.scalacheck._
import simpson.simpsonRule._
import Math._

/**
  * Created by MarioDiniz on 01/08/16.
  */
object SimpsonRuleSpecification extends Properties("SimpsonRule") {

  val interval = for {
    parts <- Gen.choose(100, 150)
    min <- Gen.choose(0, 10)
    max <- Gen.choose(min + 1, 15)
  } yield (parts * 2, min, max)

  val functions = for {
    tuple <- Gen.oneOf((sin _, (a: Int, b: Int) => cos(a) - cos(b) ),
                      ( cos _, (a: Int, b: Int) => sin(b) - sin(a) ))
  } yield (tuple)

  property("equivalent function must have the same result with 1/3 rule") =
    forAllNoShrink(interval, functions) {(interval: (Int, Int, Int),
                                          functions: (Double => Double, (Int, Int) => Double)) =>
      val (p, a, b) = interval
      val calculated = oneThirdRule(p)(a, b, functions._1)
      val result = functions._2(a, b)
      Math.max(result, calculated) - Math.min(result, calculated) < .1
    }

  property("equivalent function must have the same result with 3/8 rule") =
    forAllNoShrink(interval, functions) {(interval: (Int, Int, Int),
                                          functions: (Double => Double, (Int, Int) => Double)) =>
      val (p, a, b) = interval
      val calculated = threeOfEightRule(p * 3)(a, b, functions._1)
      val result = functions._2(a, b)
      Math.max(result, calculated) - Math.min(result, calculated) < .1
    }

  property("both methods should have slightly the same result") =
    forAllNoShrink(interval, functions) {(interval: (Int, Int, Int),
                                          functions: (Double => Double, (Int, Int) => Double)) =>
      val (p, a, b) = interval
      val calculatedOneThird = oneThirdRule(p * 3)(a, b, functions._1)
      val calculatedThreeOfEight = threeOfEightRule(p * 3)(a, b, functions._1)
      Math.max(calculatedOneThird, calculatedThreeOfEight) - Math.min(calculatedOneThird, calculatedThreeOfEight) < .001
    }
}
