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
	
	def unit[A](a: A): Rand[A] = rng => (a,rng)
	
	def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
		val (a, rng2) = s(rng)
		(f(a), rng2)
	}
}