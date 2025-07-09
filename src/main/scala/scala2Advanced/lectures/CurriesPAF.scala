package scala2Advanced.lectures

object CurriesPAF extends App {
  def superAdder(x: Int)(y: Int): Int = x + y

  val add6: Int => Int = superAdder(6)
  val add3 = superAdder(3) _ // Int => Int

  println(add3(2))

  def inc(x: Int): Int = x + 1

  List(1, 2, 3).map(inc) // map(x => inc(x))

  val simpleAddFunction = (x: Int, y: Int) => x + y

  def simpleAddMethod(x: Int, y: Int) = x + y

  def curriedAddMethod(x: Int)(y: Int) = x + y

  // add7: Int => Int = y => 7 + y

//  val add7: Int => Int = simpleAddFunction()
  val add731 = curriedAddMethod(7) _
  val add732: Int => Int = curriedAddMethod(7)
  val add721 = simpleAddMethod(7, _)
  val add711 = simpleAddFunction(7, _)
}