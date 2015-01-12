/**
 * 1/7/2015
 * @author Tobin Yehle
 */
class Location(val row: Int, val col: Int) {
  override def toString = "("+row+", "+col+")"
  override def equals(o: Any) = {
    o match {
      case other: Location => row == other.row && col == other.col
      case _ => false
    }
  }

  /**
   * Works on positive rows and columns
   * @return
   */
  override def hashCode = {
    val n = row + col
    (n*n + n) / 2 + (row - col)
  }
}
