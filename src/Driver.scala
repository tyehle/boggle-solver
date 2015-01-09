import scala.io.Source

/**
 * @author Tobin Yehle
 * 1/3/2015
 */
object Driver {
  def main(args: Array[String]):Unit = {
    val words = Source.fromFile("1000.txt").getLines().toSeq
//    val words = List("a", "ab", "abs", "add", "adds", "i", "id")

    val d = new WordTree(words)
    println(d.contains("eventualities"))
//    println(t)

    println(List(new Location(1, 2), new Location(1, 1), new Location(1, 2)).toSet.size)

    Seq(1, 2) match {
      case r :: c :: Nil => println(new Location(r, c))
      case _ => println("other")
    }
  }


  def cartesianProduct[T](xss: Seq[T]*): Seq[Seq[T]] = {
    xss match {
      case Nil => Seq(Nil)
      case hs :: tss => for(h <- hs; t <- cartesianProduct(tss:_*)) yield h +: t
    }
  }
}
