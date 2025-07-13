package scala2Advanced.lectures

object LazyEvaluation extends App {
  def sideEffectCondition: Boolean = {
    println("BOO")
    true
  }
  def simpleCondition: Boolean = true

  lazy val lazyCondition = sideEffectCondition
  println(if (simpleCondition && lazyCondition) "yes" else "no")


  def byNameMethod(n: => Int): Int = {
    lazy val t = n // call by need
    t + t + t + 1
  }
  def retrieveMagicValue = {
    println("waiting")
    Thread.sleep(2000)
    52
  }

  println(byNameMethod(retrieveMagicValue))
}
