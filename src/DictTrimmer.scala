import java.io.FileWriter

import scala.io.Source

/**
  * @author Tobin Yehle
  */
object DictTrimmer {
  def main(args: Array[String]) {
    val name = "100000"
    val length = 4

    val words = Source.fromFile(s"$name.txt").getLines()

    val fw = new FileWriter(s"$name.trimmed-$length")
    words.filter(_.length >= length).foreach(word => fw.write(word + "\n"))
    fw.close()
  }
}
