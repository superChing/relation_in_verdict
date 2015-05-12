package petproject.nlp.relation_extraction

/**
 * Author: Wei-Ching Lin
 */

case class Token(word: String,
                 lemma: String,
                 posTag: String,
                 nerTag: String)

object Token {
  /**
   * distingush en or zh by unicode range, english if true else chinese
   * @param word
   * @return
   */
  def isEnLanguage(word: String): Boolean = word.codePointAt(0) <= 0x007F
}
///**
// * note the idx is NOT for tokens .
// * @param parentNodeIdx
// * @param childNodeIdx
// * @param label
// */
//case class Arc(parentNodeIdx: Int, childNodeIdx: Int, label: Option[String] = None){
//
//  def parentTokenIdx=if (parentNodeIdx-1 <0) None else Some(parentNodeIdx-1)
//  def childTokenIdx=childNodeIdx-1
//}
//
//case class Dependency(tokens: IndexedSeq[Token], arcs: Seq[Arc]) {
//
//  def childrenOf(i: Int): Seq[Int] =
//    arcs.filter(_.parentNodeIdx == i).map(_.childNodeIdx)
//
//  def parentsOf(i: Int): Seq[Int] =
//    arcs.filter(_.childNodeIdx == i).map(_.parentNodeIdx)
//
//  def shortestDirectedPath(source: Int, dest: Int): Option[Seq[Int]] = {
//    val children = childrenOf(source)
//    if (children.contains(dest)) {
//      Some(Seq(source, dest))
//    } else {
//      val paths: Seq[Seq[Int]] = children.map(shortestDirectedPath(_, dest)).flatten.map(source +: _)
//      paths.sortBy(_.size).headOption
//    }
//  }
//
//  def shortestUndirectedPath(source: Int, dest: Int): Option[Seq[(Int, String, Int)]] = ???
//}
//
//object Dependency {
//  def apply(arcs: Seq[(Int, Int, String)]) = {
//    arcs.map(t => Arc(t._1, t._2, Option(t._3)))
//  }
//  def stanfordString2Arcs(arcs: Seq[String]) = {
//    val pattern = """(.+)\((.+)-(\d+), (.+)-(\d+)\)""".r
//    for (pattern(label, parent, parentIdx, child, childIdx) <- arcs)
//      yield Arc(parentIdx.toInt, childIdx.toInt, Option(label))
//  }
//}

/**
 *
 * a mention (span) of a named entity.
 * @note it is presumed to preserve the order of the entity mentions.
 *
 * @param entity entity resolution result. None = not yet infered.
 * @param startIdx index to the "token" that begins the mention span.
 * @param endIdxInclusive inclusive ! , index to the "token" that ends the mention span.
 * @param nerTag the class
 */
//TODO refactor it to nested class of Sentence ?
case class EntityMention(entity: Option[Long], nerTag: String, startIdx: Int, endIdxInclusive: Int) {
  // the space in both chinese and english are concerned of.
  //TODO get rid of sentence parameter.
  def mentionString(sentence: Sentence): String = {
    val tokens = mention(sentence)
    if (Token.isEnLanguage(tokens.head.word))
      tokens.map(_.word).mkString(" ")
    else
      tokens.map(_.word).mkString("")
  }
  def mention(sentence: Sentence): IndexedSeq[Token] = sentence.tokens.slice(startIdx, endIdxInclusive + 1)

}

/**
 * A directed relation mention.
 *
 * @param label relation.  None = not yet be infered.
 * @param parent the entity for the parent of the relation
 * @param child the entity for the child of the relation
 */
case class RelationMention(label: Option[Long], parent: EntityMention, child: EntityMention)

case class Sentence(tokens: IndexedSeq[Token],
                    dependencies: List[String],
                    entityMentions: IndexedSeq[EntityMention] = IndexedSeq.empty,
                    relationMentions: IndexedSeq[RelationMention] = IndexedSeq.empty)

/**
 * parsed document
 *
 * @param sentences parsed sentence seq
 * @param id specific ID of the doc. optional.
 */
case class Document(sentences: IndexedSeq[Sentence],
                    // coref: CorefAnnotation = CorefAnnotation.empty,
                    id: Option[String] = None) {
  val navigable = new NavigableDocument(this)
}


/**
 * pimp doc structure into nested structure
 */
class NavigableDocument(doc: Document) {
  lazy val sent2idx = doc.sentences.zipWithIndex.toMap
  lazy val token2sent = (for (s <- doc.sentences; t <- s.tokens) yield (t, s)).toMap
  lazy val token2idx = doc.sentences.flatMap(s => s.tokens.zipWithIndex).toMap

  implicit class NavigableToken(token: Token) {
    def document = doc
    def sentence = token2sent(token)
    def indexInSentence = token2idx(token)
  }

  implicit class NavigableSentence(sentence: Sentence) {
    def document = doc
    //    def next = document.sentences.lift(indexInDocument + 1)
    //    def prev = document.sentences.lift(indexInDocument - 1)
    def indexInDocument = sent2idx(sentence)
  }
}

