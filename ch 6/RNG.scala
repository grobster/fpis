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

}