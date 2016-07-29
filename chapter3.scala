object chapter3 {
	sealed trait List[+A]
	case object Nil extends List[Nothing]
	case class Cons[+A](head: A, tail: List[A]) extends List[A]

	object List {

		def sum(ints: List[Int]): Int = ints match {
			case Nil => 0
			case Cons(x, xs) => x + sum(xs)
		}

		def product(ds: List[Double]): Double = ds match {
			case Nil => 1
			case Cons(0.0, _) => 0
			case Cons(x, xs) => x * product(xs)
		}

		def apply[A](as: A*): List[A] = {
			if (as.isEmpty) Nil
			else Cons(as.head, apply(as.tail: _*))
		}

		def whatReturns[A](list: List[A]) = {
			list match {
				case Cons(x, Cons(2, Cons(4, _))) => x
				case Nil => 42
				case Cons(x: Int, Cons(y: Int, Cons(3, Cons(4, _)))) => x + y
				case Cons(h: Int, t: List[Int]) => h + sum(t)
				case _ => 101
			}
		}

		def tail[A](list: List[A]): List[A] = {
			list match {
				case Nil => Nil
				case Cons(_, x) => x
			}
		}

		def setHead[A](head: A, list: List[A]): List[A] = {
			list match {
				case Nil => Cons(head, Nil)
				case _ => Cons(head, list) 
			}
		}

		def drop[A](list: List[A], n: Int) : List[A] = {
			if (n <= 0) list
			list match {
				case Nil => Nil
				case Cons(_, t) => drop(t, n-1)
			}
		}

		/*
		def dropWhile[A](list: List[A], f: A => Boolean): List[A] = {
			l match {
				case Cons(h,t) if f(h) => dropWhile(t, f)
				case _ => l
			}
		}
		*/

		def append[A](a1: List[A], a2: List[A]) : List[A] = {
			a1 match {
				case Nil => a2
				case Cons(h, t) => Cons(h, append(t, a2))
			}
		}

		def init[A](l: List[A]) : List[A] = {
			l match {
				case Nil => Nil 
				case Cons(_, Nil) => Nil
				case Cons(h, t) => Cons(h, init(t))
			}
		}

		def dropWhile[A](list: List[A])(f: A => Boolean):List[A] = {
			list match {
				case Cons(h, t) if f(h) => dropWhile(t)(f)
				case _ => list
			}
		}

		def foldRight[A,B](list: List[A], valueForNil: B)(f: (A,B) => B): B = {
			list match {
				case Nil => valueForNil
				case Cons(h,t) => f(h, foldRight(t, valueForNil)(f))
			}
		}

		def sum2[A](list: List[Int]): Int = {
			foldRight(list, 0)((x,y) => x + y )
		}

		def product2(list: List[Double]): Double = {
			foldRight(list, 1.0)((x, y) => x * y)
		}

		def length[A](list: List[A]): Int = {
			foldRight(list, 0)((_, y) => y + 1)
		}

		@annotation.tailrec
		def foldLeft[A,B](l: List[A], z: B)(f: (A ,B) => B): B = {
			l match {
				case Nil => z
				case Cons(h,t) => foldLeft(t, f(h, z))(f)
			}
		}

		def sum3[A](list: List[Int]): Int = {
			foldLeft(list, 0)((x,y) => x + y )
		}

		def product3(list: List[Double]): Double = {
			foldLeft(list, 1.0)((x, y) => x * y)
		}

		def length2[A](list: List[A]): Int = {
			foldLeft(list, 0)((_, y) => y + 1)
		}

		def reverse[A](l: List[A]): List[A] = {
			foldLeft(l, Nil: List[A])((x, y) => Cons(x, y))
		}

		def appendFR[A](a: List[A], b: List[A]): List[A] = {
			foldRight(a, b)(Cons(_,_))
		}

		def appendFL[A](a: List[A], b: List[A]): List[A] = {
			foldLeft(reverse(a), b)(Cons(_,_))
		}

		def concat[A](l: List[List[A]]): List[A] = {
	 		foldRight(l, Nil:List[A])(append)
	 	}

	 	def add1(l: List[Int]): List[Int] = {
	 		l match {
	 			case Nil => Nil
	 			case Cons(h, t) => Cons(h+1, add1(t))
	 		}
	 		
	 		//foldRight(l, Nil: List[Int])((h,t) => Cons(h+1, t))
	 		//foldLeft(reverse(l), Nil: List[Int])((h,t) => Cons(h+1, t))
	 	}

	 	def doubleToString(l: List[Double]): List[String] = {
	 		foldRight(l, Nil: List[String])((h,t) => Cons(h.toString(), t))
	 	}

	 	def map[A,B](as: List[A])(f: A => B): List[B] = {
	 		foldRight(as, Nil: List[B])((h,t) => Cons(f(h), t))
	 	}

	 	def filter[A](as: List[A])(f: A => Boolean): List[A] = {
			foldRight(as, Nil: List[A])((h,t) => if (f(h)) Cons(h,t) else t)
	 	}

	 	def sumList[A](a1: List[Int], a2: List[Int]): List[Int] = {
	 		(a1,a2) match {
	 			case (Nil, _) => Nil
	 			case (_, Nil) => Nil
	 			case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, sumList(t1, t2))
	 		}
	 	}

		def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = {
			(a,b) match {
				case (Nil, _) => Nil
				case (_, Nil) => Nil
				case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
			}
		}

	}
}