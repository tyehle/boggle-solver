import scala.util.Random

/**
 * @author Tobin Yehle
 * 1/7/2015
 */
class BoggleBoard(pieces: Map[Location, String]) {
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
  def adjacent(a: Location, b: Location) = math.abs(a.row - b.row) <= 1 && math.abs(a.col - b.col) <= 1

  override def toString = {
//    pieces.toSeq.sortBy(_._1)
    ""
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

    val quTransform = (s:Char) => if(q2qu && s == 'q') "qu" else s.toString
    val letters = Random.shuffle(dice).map(s => quTransform(s(Random.nextInt(s.length))))

    val locations = Driver.cartesianProduct(0 until side, 0 until side).map{case r :: c :: Nil => new Location(r, c)}

    new BoggleBoard(locations.zip(letters).toMap)
  }
}