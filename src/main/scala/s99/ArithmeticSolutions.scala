package s99

import Solutions._
import scala.language.postfixOps

trait ArithmeticSolutions {

  // add new functions to integers
  implicit class ExtendedInt(n: Int) {

    // the primality check could be improved but oh well...
    def isPrime: Boolean = {
      if (n == 1) false
      else (2 until n-1 view) forall (n % _ != 0)
    }

    def isCoprimeTo(m: Int): Boolean = gcd(n, m) == 1
    def totient: Int = (1 until n+1).count(_.isCoprimeTo(n))
    def primeFactors: List[Int] = ???
    def primeFactorMultiplicity: List[(Int, Int)] = ???
    def primeFactorMultiplicityMap: Map[Int, Int] = ???
    def improvedTotient: Int = ???
    def listPrimesinRange(r: Range): List[Int] = ???
    def goldbach: (Int, Int) = ???
  }

  def gcd(m: Int, n: Int): Int = {
    if (n == 0) m
    else gcd(n, m % n)
  }

  def listPrimesinRange(r: Range): List[Int] = ???
  def printGoldbachList(r: Range): List[String] = ???
  def printGoldbachListLimited(r: Range, limit: Int): List[String] = ???

  // Optional but possibly useful exercise: not in original s-99 problems
  def primes: Stream[Int] = ???
}
