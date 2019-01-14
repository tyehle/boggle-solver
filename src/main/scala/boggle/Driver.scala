package boggle

import scala.annotation.tailrec
import scala.io.Source

/**
 * @author Tobin Yehle
 * 1/3/2015
 */
object Driver {
  /**
    * Run the command line interface to the solver.
    * @param args The arguments to the interface.
    */
  def main(args: Array[String]): Unit = {
    parseThenRun("100000.trimmed-4", Nil, args.toList)
  }

  /**
    * Parse command line arguments and then run the solver
    * @param wordFile The default word file to use
    * @param rows The default board
    * @param remainingArgs The remaining arguments to parse
    */
  @tailrec
  def parseThenRun(wordFile: String,
                   rows: Seq[String],
                   remainingArgs: List[String]): Unit = {
    remainingArgs match {
      case "-h" :: _     => printUsage()
      case "-?" :: _     => printUsage()
      case "--help" :: _ => printUsage()

      case "--words" :: file :: rest =>
        parseThenRun(file, rows, rest)

      case wordsEq :: rest if wordsEq.startsWith("--words=") =>
        parseThenRun(wordsEq.drop(8), rows, rest)

      case "-" :: rest =>
        printFindings(wordFile, rest)

      case badOption :: _ if badOption.startsWith("-") =>
        printUsage()

      case row :: rest =>
        parseThenRun(wordFile, rows :+ row, rest)

      case Nil =>
        printFindings(wordFile, rows)
    }
  }

  /** Print usage information and exit */
  def printUsage(): Unit = {
    println(
      """usage: java -jar boggle-solver.jar [--help|-h|-?] [--words=<path>] [<row>...]
        |
        |eg: java -jar boggle-solver.jar --words=my-words.txt thvon intpe fnaea dorlf sygrx
      """.stripMargin
    )
    System.exit(1)
  }

  /**
    * Run the solver and print the results.
    *
    * @param wordFile The file to get the words from
    * @param rows The board if the user specified it. Empty will generate a random board
    */
  def printFindings(wordFile: String, rows: Seq[String]): Unit = {
    val words = Source.fromFile(wordFile).getLines().toSeq
    val dictionary = new WordTree(words)

    val bb = if(rows.isEmpty) BoggleBoard.diceBoard(BoggleBoard.bigBoggleDice) else new BoggleBoard(rows)
    println(bb)

    val start = System.nanoTime()

    val solver = new BoggleSolver(bb, dictionary)
    val found = solver.solve().filter(_.length >= 4)

    val end = System.nanoTime()

    println(s"${found.length} words found")
    println(found.sorted.sortBy(_.length).reverse.mkString("\n"))
    println(s"Total time: ${(end-start) / 1000000000.0} seconds")
  }
}
