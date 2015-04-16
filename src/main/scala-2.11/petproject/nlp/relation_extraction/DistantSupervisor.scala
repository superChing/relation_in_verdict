package petproject.nlp.relation_extraction

/**
 * Author: Watson Lin
 * Date: 15/4/14
 */
class DistantSupervisor {
//  val kb: KB_Reader = ???

  
  def genCandidateRelation(doc: Document): Document = {
    doc.copy(sentences = doc.sentences.map(genCandidateRelation))
  }

  def genCandidateRelation(sent: Sentence): Sentence = {
    def entityPairs(entities: Seq[EntityMention]) = {
      val eWithI = entities.zipWithIndex
      for (e1 <- eWithI; e2 <- eWithI; if e1._2 != e2._2) yield {(e1, e2)}
    }

    val relations = entityPairs(sent.entityMentions).map {
      case ((e1, idx1), (e2, idx2)) => RelationMention(None, idx1, idx2)
    }
    sent.copy(relationMentions = relations.toIndexedSeq)
  }

  def annotateRelations(Sentence:Sentence)={
//       Sentence.relationMentions.foreach{}
  }






}

