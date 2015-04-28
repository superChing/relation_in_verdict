package petproject.nlp.relation_extraction

/**
 * Author: Watson Lin
 * Date: 15/4/14
 */
class DistantSupervisor() {
  val relationEx = KB.getRelationExamples()

  def annotateRelations(sentence: Sentence) = {
    val relations = sentence.relationMentions.map { r =>
      val relationLabel = relationEx.getRelation(r.parent.mentionString(sentence), r.child.mentionString(sentence))
      relationLabel match {
        case Some(pid) => r.copy(label = Option(pid))
        case None => r
      }
    }
    sentence.copy(relationMentions = relations)
  }


}
