object chapter2 {

	object highOrderFunctions {
		def forResult(name: String, n: Int, f: Int => Int): String = {
			val msg = "The %s of %d is %d."
			msg.format(name, n, f(n))
		}
	}

	object exercise1 {

		import chapter1._

		def nthFibonacci(number: Int): Int = {
			@annotation.tailrec
			def go(previous: Int = 0, current: Int = 1, nth: Int = 0): Int = {
				if (nth >= abs(number)) previous
				else 
					if (number > 0)
						go(current, previous + current, nth + 1)
					else
						go(current, previous - current, nth + 1)
			}
			go()
		}

		def test() : Unit = {
			testFibonacci(0, 0)
			testFibonacci(1, 1)
			testFibonacci(3, 2)
			testFibonacci(6, 8)
			testFibonacci(10, 55)
			testFibonacci(-1, 1)
			testFibonacci(-2, -1)
			testFibonacci(-6, -8)
			testFibonacci(-10, -55)
		}

		private def testFibonacci(value: Int, expected: Int): Unit = {
			if (nthFibonacci(value) != expected)
				println("%d nthFibonacci is different to %d".format(value, expected))
		}
	}

	object exercise2 {

		def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
			def loop(n: Int): Boolean = {
				if(n >= as.length) { 
					true
				} else {
					if (! ordered(as(n-1), as(n))) { 
						false
					} else {
						loop(n+1)
					}
				} 
			}
			return loop(1)
		}
	}

	object exercise3 {
		def curry[A,B,C](f: (A, B) => C): A => B => C = {
			(a: A) => (b: B) => f(a, b)
		}

		def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
			(a: A, b: B) => f(a)(b)
		}

		def compose[A,B,C](f: B => C, g: A => B): A => C = {
			(a: A) => f(g(a))
		}

		def composeA[A,B,C](f: A => B, g: B => C): A => C = {
			(a: A) => g(f(a))
		}
	}
}