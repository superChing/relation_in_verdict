package petproject.nlp.relation_extraction

/**
 * Author: Wei-Ching Lin
 */

case class Token(word: String,
                 lemma: String,
                 posTag: String,
                 nerTag: String)

case class Sentence(sentence: String,
                    tokens: IndexedSeq[Token],
                    dependencies: List[String],
                    entityMentions: IndexedSeq[EntityMention] = IndexedSeq.empty,
                    relationMentions: IndexedSeq[RelationMention] = IndexedSeq.empty)     {


}

/**
 * parsed document
 *
 * @param sentences parsed sentence seq
 * @param id specific ID of the doc. optional.
 */
case class Document(sentences: IndexedSeq[Sentence],
                    // coref: CorefAnnotation = CorefAnnotation.empty,
                    id: Option[String] = None) {
//  lazy val navigable = new NavigableDocument(this)
}

/**
 * mention of a named entity.
 * @note it is presumed to preserve the order of the entity mentions.
 *
 * @param entity entity resolution result. None = not yet infered.
 * @param start index to the "token" that begins the mention span.
 * @param end index to the "token" that ends the mention span.
 * @param nerTag the class
 */
case class EntityMention(entity: Option[String], nerTag: String, start: Int, end: Int)

/**
 * A directed relation mention.
 *
 * @param label relation.  None = not yet be infered.
 * @param arg1 index of entities for the parent of the relation
 * @param arg2 index of entities for the child of the relation
 */
case class RelationMention(label: Option[String], arg1: Int, arg2: Int)
