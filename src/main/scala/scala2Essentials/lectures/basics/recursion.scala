package scala2Essentials.lectures.basics

import scala.annotation.tailrec

object recursion extends App {
  @tailrec
  def str(aString: String, n: Int, acc: String = ""): String = {

    if (n <= 0) acc
    else str(aString, n - 1, acc + aString)

  }
  println(str("abc", 1))

//  def isPrime(n: Int): Boolean = {
//    def isPrimeRec(t: Int, isStillPrime: Boolean): Boolean =
//      if (isStillPrime == false) false
//      else if (t <= 1) true
//      else isPrimeRec(t - 1, n % t != 0)
//
//    isPrimeRec(n / 2, true)
//
//  }

  println(isPrime(9))

  def isPrime(n: Int): Boolean = {
    def isPrimeRec(t: Int): Boolean = {
      if (n % t == 0) false
      else if (t >= n / 2) true
      else isPrimeRec(t + 1)
    }

    isPrimeRec(2)
  }



  def fn(n: Int, m1: BigInt = 1, m2: BigInt = 1): BigInt = {

    if (n <= 2) m2
    else fn(n - 1, m2, m1 + m2 )
  }

  println(fn(400000))
}
