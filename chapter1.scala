object chapter1 {
	def abs(number: Int): Int = {
		if (number < 0)
			-number
		else
			number
	}

	def factorial(number: Int): Int = {
		@annotation.tailrec
		def go (n: Int, acc: Int): Int = {
			if (n <= 0) acc
			else go(n-1, n * acc)
		}

		go (number, 1)
	}

	private def formatAbs(number: Int): String = {
		val str = "The absolute value of %d is %d"
		str.format(number, abs(number))
	}
}