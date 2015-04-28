package petproject.nlp.relation_extraction

/**
 * Author: Wei-Ching Lin
 */

case class Token(word: String,
                 lemma: String,
                 posTag: String,
                 nerTag: String)

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
object Token {
  /**
   * distingush en or zh by unicode range, english if true else chinese
   * @param word
   * @return
   */
  def isEnLanguage(word: String): Boolean = word.codePointAt(0) <= 0x007F
}

/**
 *
 * a mention (span) of a named entity.
 * @note it is presumed to preserve the order of the entity mentions.
 *
 * @param entity entity resolution result. None = not yet infered.
 * @param start index to the "token" that begins the mention span.
 * @param last inclusive ! , index to the "token" that ends the mention span.
 * @param nerTag the class
 */
//TODO refactor it to nested class of Sentence ?
case class EntityMention(entity: Option[Long], nerTag: String, start: Int, last: Int) {
  // the space in both chinese and english are concerned of.
  //TODO get rid of sentence parameter.
  def mentionString(sentence: Sentence): String = {
    val tokens = mention(sentence)
    if (Token.isEnLanguage(tokens.head.word))
      tokens.map(_.word).mkString(" ")
    else
      tokens.map(_.word).mkString("")
  }
  def mention(sentence: Sentence): IndexedSeq[Token] = sentence.tokens.slice(start, last+1)

}

/**
 * A directed relation mention.
 *
 * @param label relation.  None = not yet be infered.
 * @param parent the entity for the parent of the relation
 * @param child the entity for the child of the relation
 */
case class RelationMention(label: Option[Long], parent: EntityMention, child: EntityMention)


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
