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

    def primeFactors: List[Int] = {
      if (n.isPrime) List(n)
      else {
        // find the first prime that divides the number
        (2 until n-1 view) find (x => x.isPrime && (n % x == 0)) match {
          case None => Nil
          case Some(x) => List(x) ::: (n / x).primeFactors
        }
      }
    }

    // let's reuse the #encode method we've done in the ListSolutions
    def primeFactorMultiplicity: List[(Int, Int)] =
      new ListsSolutions{}
      .encode(n.primeFactors)
      .map { case (k, v) => (v, k) }

    def primeFactorMultiplicityMap: Map[Int, Int] = n.primeFactorMultiplicity.toMap

    def improvedTotient: Int =
      n.primeFactorMultiplicity.foldLeft(1) { case (acc, n) => acc * (n._1 - 1) * Math.pow(n._1, n._2 - 1).toInt }

    def goldbach: (Int, Int) =
      listPrimesinRange(2 to n).find { i => (n - i).isPrime }.map(i => (i, n - i)).get
  }

  def gcd(m: Int, n: Int): Int = {
    if (n == 0) m
    else gcd(n, m % n)
  }

  def listPrimesinRange(r: Range): List[Int] = r.filter(_.isPrime).toList

  def printGoldbachList(r: Range): List[String] = r
    .filter(_ % 2 == 0)
    .map { n =>
      val (a, b) = n.goldbach
      s"${n} = ${a} + ${b}"
    }.toList

  def printGoldbachListLimited(r: Range, limit: Int): List[String] = r.foldRight(List[String]()) { case (n, acc) =>
    if (n % 2 == 0) {
      val (a, b) = n.goldbach
      if (a > limit) s"${n} = ${a} + ${b}" :: acc
      else acc
    } else acc
  }

  // Optional but possibly useful exercise: not in original s-99 problems
  def primes: Stream[Int] = ???
}
