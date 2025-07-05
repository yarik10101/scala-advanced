package scala2Essentials.lectures.basics

object HelloWorldApp extends App {

  def i(a: BigInt): BigInt =
    if (a <= 0) 1
    else a * i(a - 1)
  println(i(100))

  def fn(p: Int): Int =
    if (p <= 2) 1 else fn(p - 2) + fn(p - 1)
  println(fn(10))

  def fac(n: Int): BigInt = {
    def fachelp(x: Int, acc: BigInt): BigInt =
      if (x <= 1) acc
      else fachelp(x-1, x * acc)
    fachelp(n, 1)
  }
println(fac(100000))
}
