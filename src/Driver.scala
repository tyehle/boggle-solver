import scala.io.Source

/**
 * @author Tobin Yehle
 * 1/3/2015
 */
object Driver {
  def main(args: Array[String]):Unit = {
    val words = Source.fromFile("100000.txt").getLines().toSeq
//    val words = List("a", "ab", "abs", "add", "adds", "i", "id")

    val dictionary = new WordTree(words)
    println(dictionary.contains("eventualities"))
//    println(t)

    println(List(new Location(1, 2), new Location(1, 1), new Location(1, 2)).toSet.size)

    Seq(1, 2) match {
      case r :: c :: Nil => println(new Location(r, c))
      case _ => println("other")
    }

//    println(cartesianProduct(0 to 1, 0 to 1))
    val bb = BoggleBoard.diceBoard(BoggleBoard.bigBoggleDice)
    println(bb)
    val solver = new BoggleSolver(bb, dictionary)
    val found = solver.solve()
    println(found.length+" words found")
    println(found.mkString("\n"))
  }


  def cartesianProduct[T](xss: Seq[T]*): Seq[Seq[T]] = {
    xss match {
      case Nil => Seq(Nil)
      case hs +: tss => for(h <- hs; t <- cartesianProduct(tss:_*)) yield h +: t
    }
  }
}
