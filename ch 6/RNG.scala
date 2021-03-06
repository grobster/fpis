trait RNG {
	def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
	def nextInt: (Int, RNG) = {
		val newSeed = (seed * 0x5DEECE66DL) + 0xFFFFFFFFFFFFL
		val nextRNG = SimpleRNG(newSeed)
		val n = (newSeed >>> 16).toInt
		(n, nextRNG)
	}
}

object RNG {	
	def nonNegativeInt(rng: RNG): (Int, RNG) = {
		val (i,r) = rng.nextInt
		(if(i < 0) -(i + 1) else i, r)
	}
	
	def double(rng: RNG): (Double, RNG) = {
		val (i,r) = nonNegativeInt(rng)
		(i / (Int.MaxValue.toDouble + 1), r)
	}
	
	def intDouble(rng: RNG): ((Int,Double), RNG) = {
		val (i,r) = rng.nextInt
		val (d,r2) = double(rng)
		((i,d), r2)
	}
	
	def doubleInt(rng: RNG): ((Double,Int), RNG) = {
		val ((i,d), r) = intDouble(rng)
		((d,i),r)
	}
	
	def double3(rng: RNG): ((Double, Double, Double), RNG) = {
		val (d,r) = double(rng)
		val (d1, r1) = double(r)
		val (d2, r2) = double(r1)
		((d,d1,d2), r2)
	}
	
	def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
		if(count == 0)
			(List(), rng)
		else {
			val (i,r) = rng.nextInt
			val (i2,r2) = ints(count - 1)(r)
			(i :: i2, r2)
		}
	}
	
	type Rand[+A] = RNG => (A, RNG)
	
	val int: Rand[Int] = _.nextInt
	
	def unit[A](a: A): Rand[A] = rng => (a,rng)
	
	def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
		val (a, rng2) = s(rng)
		(f(a), rng2)
	}
	
	def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)
	
	def double2: Rand[Int] = map(nonNegativeInt)(_ / (Int.MaxValue + 1))
	
	def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = rng => {
		val (a,r1) = ra(rng)
		val (b, r2) = rb(r1)
		(f(a,b), r2)
	}
	
	def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra,rb)((_,_))
	
	def randIntDouble: Rand[(Int,Double)] = both(int,double)
	
	val randDoubleInt: Rand[(Double, Int)] = both(double, int)
	
	def sequence[A](li: List[Rand[A]]): Rand[List[A]] = li.foldRight(unit(List[A]()))((a,b) => map2(a,b)(_ :: _))
}