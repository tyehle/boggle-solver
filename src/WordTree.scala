import scala.collection.mutable

/**
 * Represents words as a tree
 * @author Tobin Yehle
 * 1/3/2015
 */
class WordTree(children: mutable.Map[Char, WordTree], isWord: Boolean) {
  /**
   * Constructs a word tree using a list of words and a starting letter.
   * @param words The list of words to construct the rest of the tree
   */
  def this(words: Seq[String]) = {
    this(mutable.Map[Char, WordTree](), words.contains(""))

    val firstLetterSplit = mutable.HashMap[Char, mutable.Buffer[String]]()
    for(word <- words) {
      if(word != "") {
        if(!firstLetterSplit.contains(word.head))
          firstLetterSplit.update(word.head, mutable.Buffer())

        firstLetterSplit(word.head).prepend(word.tail)
      }
    }

    children ++= firstLetterSplit.map{case (letter, remaining) => letter -> new WordTree(remaining)}
  }

  def contains(word: String): Boolean = {
    if(word == "") isWord
    else if(children.keySet.contains(word.head)) children(word.head).contains(word.tail)
    else false
  }

  override def toString: String = toString("", last=true, letter=' ')

  /**
   * Does a pretty print of this tree
   * @param prefix The prefix to add to all lines
   * @param last If this is the last item in the super tree
   * @return A pretty string representation of the object
   */
  def toString(prefix: String, last: Boolean, letter: Char): String = {
    val thisNode = prefix+" +--"+letter+{if(isWord) "-<>" else ""}+"\n"

    if(children == null || children.isEmpty)
      thisNode
    else {
      val prepend = if(last) "   " else " | "
      thisNode +
        children.init.map{case (char, tree) => tree.toString(prefix + prepend, last=false, char)}.mkString("") +
        children.values.last.toString(prefix + prepend, last=true, children.keys.last)
    }
  }
}
