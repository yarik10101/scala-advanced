package scala2Essentials.lections

import scala.math.BigDecimal.int2bigDecimal

object Exceptions extends App{
class OverflowException(gg: String = "gg") extends Throwable
class UnderflowException extends Throwable
class MathCalculationException extends Throwable

// throw new OutOfMemoryError("out of memory")
//  try {
//    throw new OutOfMemoryError
//    throw new StackOverflowError
//  }
//  catch {
//    case e:OutOfMemoryError => println("y")
//    case p:StackOverflowError => println("yy")
//  }
object PocketCalculator {
  def add(x: Int, y: Int): Int = {
    if (x > 0 && y > 0 && x + y < 0) throw new OverflowException()
    else if (x < 0 && y < 0 && x + y > 0) throw new UnderflowException()
    else x + y
  }
  def subtract(x: Int, y: Int): Int = add(x, -y)
  def multiply(x:Int, y: Int): Int = {
    if (x == 0 || y == 0) 0
    else if (x / y > 0) {
      if (x > 0 && Int.MaxValue / x <= y) throw new OverflowException()
      else if (x < 0 && Int.MaxValue / x >= y) throw new UnderflowException()
      else x * y
    }
    else if (x / y < 0 && ((x < 0 && Int.MaxValue / x >= -y) || (x > 0 && Int.MaxValue / x <= -y))) throw new UnderflowException()
    else x * y

  }


  def divide(x: Double, y: Double): Double = {
    if (y == 0) throw new MathCalculationException
    else x / y
  }


}
println(PocketCalculator.add(Int.MaxValue,Int.MaxValue))
}
