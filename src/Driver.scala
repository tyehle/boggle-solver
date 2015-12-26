import scala.io.Source

/**
 * @author Tobin Yehle
 * 1/3/2015
 */
object Driver {
  def main(args: Array[String]):Unit = {
    val words = Source.fromFile("100000.trimmed-4").getLines().toSeq

    val dictionary = new WordTree(words)

    val bb = if(args.nonEmpty) new BoggleBoard(args.toSeq) else BoggleBoard.diceBoard(BoggleBoard.bigBoggleDice)
    println(bb)
    val solver = new BoggleSolver(bb, dictionary)
    val found = solver.solve().filter(_.length >= 4)
    println(found.length+" words found")
    println(found.sorted.sortBy(_.length).reverse.mkString("\n"))
  }


  def cartesianProduct[T](xss: Seq[T]*): Seq[Seq[T]] = {
    xss match {
      case Nil => Seq(Nil)
      case hs +: tss => for(h <- hs; t <- cartesianProduct(tss:_*)) yield h +: t
    }
  }
}
