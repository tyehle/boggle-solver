package boggle

import scala.annotation.tailrec

/**
 * Represents words as a tree
 * @author Tobin Yehle
 * 1/3/2015
 */
final class WordTree(children: Map[Char, WordTree], val isWord: Boolean) {
  /**
   * Constructs a word tree using a list of words.
   * @param words The list of words to construct the rest of the tree
   */
  def this(words: Seq[String]) = {
    this(words.filter(_.nonEmpty).groupBy(_.head).mapValues(words => new WordTree(words.map(_.tail))),
         words.contains(""))
  }

  @tailrec
  def contains(word: String): Boolean = {
    if(word == "") isWord
    else if(children.keySet.contains(word.head)) children(word.head).contains(word.tail)
    else false
  }

  @tailrec
  def subTree(prefix: String): Option[WordTree] = {
    if(prefix == "") Some(this)
    else if(!children.keySet.contains(prefix.head)) None
    else children(prefix.head).subTree(prefix.tail)
  }

  def apply(letter: Char): WordTree = children(letter)

  def letterSet: Set[Char] = children.keySet

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
