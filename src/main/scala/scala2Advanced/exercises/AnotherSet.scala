package scala2Advanced.exercises

trait AnotherSet[A] extends (A => Boolean) {
  def contains(x: A): Boolean = apply(x)

  def +(y: A): AnotherSet[A] = new AnotherSet[A] {
    def apply(x: A): Boolean = contains(x) || x == y
  }

  def ++(anotherSet: AnotherSet[A]): AnotherSet[A] = new AnotherSet[A] {
    def apply(x: A): Boolean = this.contains(x) || anotherSet.contains(x)
  }

  def map[B](f: A => B): AnotherSet[B] = throw new Exception("no map for u")

  def flatMap[B](f: A => AnotherSet[B]): AnotherSet[B] = throw new Exception("no fMap for u")

  def filter(predicate: A => Boolean): AnotherSet[A] = new AnotherSet[A] {
    def apply(x: A): Boolean = predicate(x)
  }

  def foreach(f: A => Unit): Unit = throw new Exception("go foreach yourself")

}
object HelpMe extends App {
  val myset = new AnotherSet[Int] {
    override def apply(x: Int): Boolean = x % 2 == 0 || x == 3
  }
  println( )
}