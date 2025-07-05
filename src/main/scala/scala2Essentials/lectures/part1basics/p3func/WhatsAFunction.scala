package scala2Essentials.lectures.part1basics.p3func

object WhatsAFunction extends App {
  val concatenator: (String, String) => String = new Function2[String, String, String] {
    override def apply(str1: String, str2: String): String =
      str1 + str2
  }
  val parovoz: Function1[Int, Function1[Int, Int]] = new Function1[Int, Function1[Int, Int]] {
    override def apply(int: Int): Function1[Int, Int] = new Function1[Int, Int] {
      override def apply(intx: Int): Int = ???
    }
  }
  val mySuperAdder: Int => Int => Int = int1 => int2 => int1 + int2

  {
    def gavno(string: String): String = ???
  }
}

