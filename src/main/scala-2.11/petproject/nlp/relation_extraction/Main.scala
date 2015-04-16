package petproject.nlp.relation_extraction

/**
 * Author: Watson Lin
 * Date: 15/4/11
 */
class Main extends App {
  val corpus: List[(String, String)] = List()
  val corpusParsed: List[Document] = corpus.map { case (doc, docID) =>
    val docParsed = DocumentParcer.using("stanfordNLP").parseDocument(doc, docID)

    //      .sentences.zipWithIndex.map{
    //      case (sentParse, sentIdx) =>
    //        val sentID = s"$docID@$sentIdx"
    //        Map(
    //          "document_id" -> docID,
    //          "sentence_id" -> sentID,
    //          "sentence" -> sentParse.sentence,
    //          "words" -> sentParse.words,
    //          "pos_tags" -> sentParse.posTags,
    //          "lemma" -> sentParse.lemma,
    //          "dependencies" -> sentParse.deps,
    //          "ner_tags" -> sentParse.nerTags,
    //          "sentence_offset" -> sentIdx
    //        )
    //    }
    docParsed
  }



}
