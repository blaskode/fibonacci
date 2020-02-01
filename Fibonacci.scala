
/**Practice using recursive/functional techniques. How many ways can I 
compute the Fibonacci sequence? Let me count the ways! */

object Fibonacci {

	/*Produces the nth Fibonacci number starting at n = 0 */
	def fibonacci(n: Int): Int = {
		def go(prev: Int, prev_prev: Int, count: Int): Int = {
			if (count == n - 2) prev_prev + prev
			else go(prev + prev_prev, prev, count + 1)
		}
		if (n == 0) {
			0
		} else if (n == 1) {
			1
		} else {
			go(1, 0, 0)
		}
	}

	/*Produces sequence as an array. This is a cheat because it uses mutation...*/
	def fib_seq(n: Int) : Array[Int] = {
		var array = Array[Int]()
		def go(x: Int) : Array[Int] = {
			if (x == n){
				array 
			} else {
				array = array :+ fibonacci(x)
				go(x + 1)
			}
		}
		go(0)
	}

	/*No cheat, no mutation, produces a List*/
	def fib_seq2(n: Int) : List[Int] = {
		val num_list = List[Int]()
		def go(x: Int, l: List[Int]) : List[Int] = {
			if (x == 0) {
				l
			} else {
				go(x - 1, fibonacci(x - 1)::l)
			}
		}
		go(n, num_list)
	}

	/*This adds the last two elements of the list without calling
		an outside function...*/

	def fib_seq3(n: Int) : List[Int] = {
		val start = List[Int](0, 1)
		def go(x : Int, l : List[Int]) : List[Int] = {
			if (x == 2) {
				l
			} else {
				val e1 = l(l.length - 1)
				val e2 = l(l.length - 2)
				val sum = e1 + e2
				go(x - 1, l:::List(sum))
			}
		}
		go(n, start)
	}

 












