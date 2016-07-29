object simpsonRule {
	def oneThirdRule(strips: Int)(min: Double, max: Double, f: Double => Double): Double = {
		require(strips % 2 == 0, "Strips must be an even number.")

		val step = Math.abs(max - min) / strips

		def k(i:Int): Int = {
			i match {
				case `strips` | 0 => 1
				case _ if (i % 2 == 0) => 2
				case default => 4
			}
		}

		@annotation.tailrec
		def loop (x: Double, i: Int, acc: Double = 0): Double = {
			if (x > max) { acc } else { 
				loop(x + step, i + 1, (k(i) * f(x)) + acc)
			}
		}

		loop(min, 0) * (step / 3)
	}

	def threeOfEightRule(strips: Int)(min: Double, max: Double, f: Double => Double): Double = {
		require(strips % 2 == 0, "Strips must be an even number.")
		require(strips % 3 == 0, "Strips must be a multiple of 3.")

		val step = Math.abs(max - min) / strips

		def k(i:Int): Int = {
			i match {
				case `strips` | 0 => 1
				case _ if (i % 3 == 0) => 2
				case default => 3
			}
		}

		def loop(x: Double, i: Int, acc: Double = 0): Double = {
			if (x > max) { acc } else { 
				loop(x + step, i + 1, (k(i) * f(x)) + acc)
			}
		}

		loop(min, 0) * (3 * step / 8)
	}
}