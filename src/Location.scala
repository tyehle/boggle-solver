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
  override def hashCode = col.hashCode + 29*row.hashCode // hopefully there will not be more than 29 columns
}
