package boggle

import scala.util.Random

/**
 * @author Tobin Yehle
 * 1/7/2015
 */
class BoggleBoard(val pieces: Map[Location, String]) {

  /**
    * Build a board from a number of lines.
    * @param lines The characters in each line
    */
  def this(lines: Seq[String]) = {
    this {
      (for {
        (line, lineNum) <- lines.zipWithIndex
        (char, charIndex) <- line.zipWithIndex
      } yield new Location(lineNum, charIndex) -> char.toString).toMap
    }
  }

  /**
   * Tests if a path drawn through the board is a valid path.
   * @param path The path, represented as a sequence of locations
   * @return If the path is valid on this board
   */
  def isValid(path: Seq[Location]): Boolean = {
    if (path.length == path.toSet.size && path.forall(pieces.keySet.contains))
      path.zip(path.tail).forall((adjacent _).tupled)
    else
      false
  }

  def getWord(path: Seq[Location]): Option[String] = {
    if(isValid(path))
      Some(path.map(pieces).mkString)
    else
      None
  }

  /**
   * Checks if two locations are considered adjacent on the board.
   * @param a The first location
   * @param b The second location
   * @return If the two given locations are adjacent
   */
  def adjacent(a: Location, b: Location): Boolean =
    math.abs(a.row - b.row) <= 1 && math.abs(a.col - b.col) <= 1

  override def toString: String = {
    val letters = pieces.toSeq.sortBy{case (loc, _) => (loc.row, loc.col)}.map(_._2)
    val side = Math.sqrt(letters.length).asInstanceOf[Int]

    val firstLine = "┌" + List.fill(side)("───").mkString("┬") + "┐\n"
    val lineBreak = "├" + List.fill(side)("───").mkString("┼") + "┤\n"
    val lastLine = "└" + List.fill(side)("───").mkString("┴") + "┘\n"

    firstLine +
      letters.grouped(side).map{
                                 row => "│" + row.map{
                                                       l => (" "+l+" ").take(3)
                                                     }.mkString("│") + "|\n"
                               }.mkString(lineBreak) +
      lastLine
  }
}

object BoggleBoard {
  val bigBoggleDice = List("aaafrs",
                           "aaeeee",
                           "aafirs",
                           "adennn",
                           "aeeeem",

                           "aeegmu",
                           "aegmnn",
                           "afirsy",
                           "bjkqxz",
                           "ccenst",

                           "ceiilt",
                           "ceilpt",
                           "ceipst",
                           "ddhnot",
                           "dhhlor",

                           "dhlnor",
                           "dhlnor",
                           "eiiitt",
                           "emottt",
                           "ensssu",

                           "fiprsy",
                           "gorrvw",
                           "iprrry",
                           "nootuw",
                           "ooottu")

  val classicBoggleDice = List("AACIOT",
                               "ABILTY",
                               "ABJMOQ",
                               "ACDEMP",
                               "ACELRS",
                               "ADENVZ",
                               "AHMORS",
                               "BIFORX",
                               "DENOSW",
                               "DKNOTU",
                               "EEFHIY",
                               "EGKLUY",
                               "EGINTV",
                               "EHINPS",
                               "ELPSTU",
                               "GILRUW")

  val newBoggleDice = List("AAEEGN",
                           "ABBJOO",
                           "ACHOPS",
                           "AFFKPS",
                           "AOOTTW",
                           "CIMOTU",
                           "DEILRX",
                           "DELRVY",
                           "DISTTY",
                           "EEGHNW",
                           "EEINSU",
                           "EHRTVW",
                           "EIOSST",
                           "ELRTTY",
                           "HIMNUQ",
                           "HLNNRZ")

  /**
   * Builds a boggle board by shuffling and rolling a number of dice.
   * @param dice The dice to roll. Each letter represents one side of a die.
   * @param q2qu If a q on a die should be treated as a qu instead. Default true.
   * @return A new boggle board constructed from the given dice.
   */
  def diceBoard(dice: List[String], q2qu:Boolean=true): BoggleBoard = {
    val root = Math.sqrt(dice.length)
    if(root != Math.rint(root)) {
      throw new IllegalArgumentException("Must have a square number of dice: "+dice.length)
    }
    val side = root.asInstanceOf[Int]

    val quTransform = (s:Char) => if(q2qu && s == 'q') "qu" else s.toLower.toString
    val letters = Random.shuffle(dice).map(s => quTransform(s(Random.nextInt(s.length))))

    val locations = for { r <- 0 until side; c <- 0 until side } yield new Location(r, c)

    new BoggleBoard(locations.zip(letters).toMap)
  }
}
