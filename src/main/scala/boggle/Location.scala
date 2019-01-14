package boggle

/**
 * 1/7/2015
 * @author Tobin Yehle
 */
class Location(val row: Int, val col: Int) {
  override def toString: String = "(" + row + ", " + col + ")"
  override def equals(o: Any): Boolean = {
    o match {
      case other: Location => row == other.row && col == other.col
      case _ => false
    }
  }

  /**
   * Works on positive rows and columns
   * @return
   */
  override def hashCode: Int = {
    val n = row + col
    (n*n + n) / 2 + (row - col)
  }
}
