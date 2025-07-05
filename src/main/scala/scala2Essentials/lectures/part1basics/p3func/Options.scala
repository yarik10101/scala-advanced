package scala2Essentials.lectures.part1basics.p3func

import scala.util.Random

object Options extends App {
  val config: Map[String, String] = Map (
  "host" -> "111.11.11.1",
    "port" -> "11"
  )

  class Connection {
    def connect = println("Connected")
  }
  object Connection {
    val random = new Random(System.nanoTime())

    def apply(host: String, port: String): Option[Connection] =
      if (random.nextBoolean()) Some(new Connection)
      else None
  }
  val newConfig: Option[Connection] = config.get("host").flatMap(x => config.get("port").flatMap(y => Connection(x, y)))
  newConfig.foreach(z => z.connect)
  for {
    h <- config.get("host")
    p <- config.get("port")
    c <- Connection(h, p)
  }
    c.connect


}

