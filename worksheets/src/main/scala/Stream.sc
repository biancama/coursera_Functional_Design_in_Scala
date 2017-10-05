def isPrime(x: Int) = {
  def isPrime(x: Int, d: Int): Boolean = if (d == 1) true
  else if (x % d == 0) false
  else isPrime(x, d-1)
  isPrime(x, x-1)
}
val firstPrime = ((100 to 1000) filter isPrime) (1)

val firstPrimeWithStream = ((100 to 1000).toStream filter isPrime)(1)

def streamRange(lo: Int, hi: Int) : Stream[Int] =
  if (lo >= hi) Stream.empty
  else Stream.cons(lo, streamRange(lo + 1, hi))

def listRange(lo: Int, hi: Int): List[Int] =
  if (lo >= hi) Nil
  else lo :: listRange(lo +1, hi)


listRange(1, 10)

streamRange(1, 10)

streamRange(1, 10).tail

val stream = 1 #:: streamRange(2, 10)  // use of #:: for cons






