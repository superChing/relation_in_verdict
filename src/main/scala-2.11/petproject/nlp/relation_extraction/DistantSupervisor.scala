package petproject.nlp.relation_extraction


/**
 * Author: Watson Lin
 */


class DistantSupervisor(relationRDF: KB.RelationTupleSet) {
  def annotateRelations(document: Document): Document = {
    document.copy(sentences = document.sentences.map(annotateRelations))
  }

  def annotateRelations(sentence: Sentence): Sentence = {
    val relations = sentence.relationMentions.map { r =>

      val isCandidate = (getString: EntityMention => String) => relationRDF.isRelationCandidate(
        getString(r.getParentEntity(sentence)),
        getString(r.getChildEntity(sentence))
      )

      isCandidate(_.zhString(sentence)) || isCandidate(_.enString(sentence)) match {
        case true => r.copy(label = Some(relationRDF.relationID))
        case _    => r
      }

    }
    sentence.copy(relationMentions = relations)
  }
}

object DistantSupervisor {
  def apply(relationID: Int) = new DistantSupervisor(KB.getRDF(relationID, None))
}