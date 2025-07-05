package scala2Advanced.lectures

import scala.util.Random

object DarkSugars extends App {

  def singleArgMethod(arg: Int): String = s"$arg gg"
  val random = new Random(System.nanoTime())
  val description = singleArgMethod {
    val x = -42
    x match {
      case x if x < 0 => 0
      case _ => 1
    }
  }
  val mishapidor = Array(1, 2, 34)
  mishapidor(2) = 100
  println(mishapidor.mkString("Array(", ", ", ")"))
  println(description)

}
