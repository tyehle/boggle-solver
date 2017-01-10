package boggle

/**
 * 1/9/2015
 * @author Tobin Yehle
 */
class BoggleSolver(board: BoggleBoard, dictionary: WordTree) {
  def solve(): Seq[String] = {
    board.pieces.keySet.foldLeft(Seq.empty[String])((words, loc) => words ++ findWordsAt(loc)).distinct
  }

  def findWordsAt(loc: Location):Seq[String] = {
    dictionary.subTree(board.pieces(loc)) match {
      case Some(subDict) => findAllWords(Seq(loc), subDict)
      case None => Nil
    }
  }

  def findAllWords(prevPath: Seq[Location], subDict: WordTree): Seq[String] = {
    val subwords = pathStep(prevPath).flatMap(step => subDict.subTree(board.pieces(step)) match {
      case Some(subSubDict) => findAllWords(prevPath :+ step, subSubDict)
      case None => Nil
    })

    val thisLetter = board.pieces(prevPath.last)

    {if(subDict.isWord) Seq(thisLetter) else Nil} ++ subwords.map(word => thisLetter + word)
  }

  def pathStep(path: Seq[Location]): Seq[Location] = {
    board.pieces.keys.filter(l => board.adjacent(l, path.last) && !path.contains(l)).toSeq
  }
}
