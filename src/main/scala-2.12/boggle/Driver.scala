package boggle

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

    val start = System.nanoTime()

    val solver = new BoggleSolver(bb, dictionary)
    val found = solver.solve().filter(_.length >= 4)

    val end = System.nanoTime()

    println(s"${found.length} words found")
    println(found.sorted.sortBy(_.length).reverse.mkString("\n"))
    println(s"Total time: ${(end-start) / 1000000000.0} seconds")
  }

  def cartesianProduct[T](groups: Seq[T]*): Stream[Seq[T]] = {
    if(groups.isEmpty) Stream(Seq.empty)
    else {
      groups.head.flatMap(head => cartesianProduct(groups.tail:_*).map(tail => head +: tail))(collection.breakOut)
    }
  }
}
