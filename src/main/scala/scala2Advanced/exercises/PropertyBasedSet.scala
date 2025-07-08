package scala2Advanced.exercises

trait PropertyBasedSet[A] extends (A => Boolean) {
  def contains(x: A): Boolean = apply(x)

  def +(y: A): PropertyBasedSet[A] = new PropertyBasedSet[A] {
    def apply(x: A): Boolean = contains(x) || x == y
  }

  def ++(anotherSet: PropertyBasedSet[A]): PropertyBasedSet[A] = new PropertyBasedSet[A] {
    def apply(x: A): Boolean = this.contains(x) || anotherSet.contains(x)
  }

  def map[B](f: A => B): PropertyBasedSet[B] = throw new Exception("no map for u")

  def flatMap[B](f: A => PropertyBasedSet[B]): PropertyBasedSet[B] = throw new Exception("no fMap for u")

  def filter(predicate: A => Boolean): PropertyBasedSet[A] = new PropertyBasedSet[A] {
    def apply(x: A): Boolean = predicate(x)
  }

  def foreach(f: A => Unit): Unit = throw new Exception("go foreach yourself")

}
object HelpMe extends App {
  val myset = new PropertyBasedSet[Int] {
    override def apply(x: Int): Boolean = x % 2 == 0 || x == 3
  }
  println( )
}