package petproject.nlp.relation_extraction


/**
 * Author: Wei-Ching Lin
 */


/**
 * parse the doc string into [[Document]]
 */
trait DocumentParcer {
  val NoneNerClass: String
  val NounClasses: Set[String]
  def parseDocument(doc: String, docID: String = null): Document
  /**
   * @see [[DocumentParcer.annotateChineseEntityMention(sent: Sentence):Sentence*]]
   * @param doc
   * @return
   */
  def annotateChineseEntityMention(doc: Document): Document = {
    doc.copy(sentences = doc.sentences.map(annotateChineseEntityMention))
  }
  /**
   * it treats all ner tag but "Others class" as part of some entity.
   * 中文沒有大寫，常常把專有名詞歸類為一般名詞，例如台北醫院(Taipei Hospital) 一整個是一個詞，但可能分詞分成兩個詞，然後NER把醫院被歸類為一般名詞(不屬於專有名詞 i.e. NER class)。這是分詞器太爛，因為台北的醫院，和台北醫院差很多好不好，中文很少會忽略 ”的“。
   * Concatenate the consecutive ner tags and nouns and regard them as an entity . Why Nouns in addition, beacause the concecutive NER tag with nouns as whole is usually an entity. (ex:  Google car --the last word in entity maybe a noun but a NER class.)
   * @note maybe the words all are noun but NER class, we exclude such case so as to focus on more unambiguous "named"
   *       entity that was in ner class.
   * @note it do not make `entity resolution`, the entity is None.
   *       you can distinguish entities by raw mention string, just calling EntityMention#mentionString.
   * @note the entityMentions is ordered as the ordering in sentence. //this assumption maybe deleted in the future ?
   * @param sent sentence that need for finding entities
   * @return sentence with entity mentions
   */
  def annotateChineseEntityMention(sent: Sentence): Sentence = {
    val isNoun = (token: Token) => token.nerTag != NoneNerClass || NounClasses.contains(token.posTag)
    def getEntityMentions(tokenWithIdx: Seq[(Token, Int)]): Seq[EntityMention] = tokenWithIdx match {
      case seq @ ((token, idx) +: xs) if isNoun(token) =>
        val segment = seq.takeWhile {case (t, i) => isNoun(t) }
        val headIdx = segment.head._2
        val endIdx = segment.last._2 + 1

        // take the last ner tag as entity tag
        // and filter out entity not in ner class (that is , no ner class in its tags)
        val tag = segment.find {case (t, i) => token.nerTag != NoneNerClass }.map(_._1.nerTag)
        if (tag.isEmpty)
          getEntityMentions(seq.drop(segment.length))
        else
          EntityMention(None, tag.get, headIdx, endIdx) +: getEntityMentions(seq.drop(segment.length))

      case seq @ (x, idx) +: xs if x.nerTag == NoneNerClass => getEntityMentions(xs)
      case Nil                                              => Nil
    }

    sent.copy(entityMentions = getEntityMentions(sent.tokens.zipWithIndex).toIndexedSeq)
  }


}


/**
 * factory object
 */
object DocumentParcer {
  val default: String = "FNLP"
  def apply(which: String): DocumentParcer = using(which)
  def parseDocument(doc: String, docID: String): Document = using(default).parseDocument(doc, docID)
  def using(name: String): DocumentParcer = name.toLowerCase match {
    case "fnlp" => FNLP
    //    case "stanfordnlp" | "stanford" => StanfordNLP
  }


  def genCandidateRelation(doc: Document): Document = {
    doc.copy(sentences = doc.sentences.map(genCandidateRelation))
  }


  /**
   * generate all possible relations from entities
   * and annotate relation label as None
   * @param sent
   * @return
   */
  def genCandidateRelation(sent: Sentence): Sentence = {
    def entityPairs(entities: Seq[EntityMention]) = {
      val eWithI = entities.zipWithIndex
      if (INVERSE_ENTITY_ORDER)
        for (e1 <- eWithI; e2 <- eWithI; if e1._2 != e2._2) yield {(e1, e2)}
      else
        for (e1 <- eWithI; e2 <- eWithI; if e1._2 < e2._2) yield {(e1, e2)}
    }

    //if too many entity, we skip this sentence.
    //TODO logging
    if (sent.entityMentions.size <= 7) {
      val relations = entityPairs(sent.entityMentions).map {
        case ((e1, idx1), (e2, idx2)) => RelationMention(None, idx1, idx2)
      }
      sent.copy(relationMentions = relations.toIndexedSeq)
    } else {
      sent
    }
  }

  def split2ChineseSentence(doc: String) = {
    val pattern = """[!?！？]+|[。]+""".r
    val idx = (pattern findAllMatchIn doc).map(words => words.end)
    val idx4sent = (Iterator(0) ++ idx).sliding(2, 1)
    idx4sent.map(pair => doc.substring(pair.head, pair(1))).map(_.trim)
  }
}

///**
// * make sure JVM option -Xmx2g
// * I eat simple chinese
// */
//object StanfordNLP extends DocumentParcer {
//
//  import edu.stanford.nlp.ling.CoreAnnotations._
//  import edu.stanford.nlp.pipeline._
//
//  override val NoneNerClass: String = "O"
//  override val NounClasses: String = ???
//  val nerClasses = Set("PERSON", "ORGANIZATION", "GPE", "LOC", "MISC")
//
//  val props = new Properties()
//  props.load(inputStream.getOrElse(throw new FileNotFoundException("property file not found in the classpath")))
//  props.put("threads", Runtime.getRuntime.availableProcessors.toString)
//  val inputStream = Option(getClass.getClassLoader.getResourceAsStream("StanfordCoreNLP-chinese.properties"))
//  //need add classpath to the models
//  val pipeline = new StanfordCoreNLP(props)
//  def parseDocument(doc: String, docID: String = null) = {
//    val document = new Annotation(doc)
//    pipeline.annotate(document)
//
//    // val docCoref = document.get(classOf[CorefChainAnnotation]) //coreference
//    val sentenceAnnos = document.get(classOf[SentencesAnnotation])
//
//    val sentences = sentenceAnnos.zipWithIndex.map {
//      case (sentenceAnno, idx) =>
//        val tokenAnnos = sentenceAnno.get(classOf[TokensAnnotation])
//        val tokens = tokenAnnos.map {
//          tokenAnno =>
//            val word = tokenAnno.get(classOf[TextAnnotation])
//            val lemma = tokenAnno.get(classOf[LemmaAnnotation])
//            val pos = tokenAnno.get(classOf[PartOfSpeechAnnotation])
//            val ner = tokenAnno.get(classOf[NamedEntityTagAnnotation])
//            Token(word, pos, ner, Some(lemma))
//        }.toVector
//
//        //        val sentence = sentenceAnno.get(classOf[TextAnnotation])// sentence text
//        //        val dep = sentenceAnno.get(classOf[CollapsedCCProcessedDependenciesAnnotation])
//        //        dep.toString(SemanticGraph.OutputFormat.LIST).lines.toList
//        Sentence(tokens)
//    }.toVector
//
//    Document(sentences, Option(docID))
//  }
//  /**
//   * just a dummy wrapper.
//   * Why this?
//   * cuz the ssplit annotator of CoreNLP pipeline is not functioning for chinese.
//   * I define one myself and hardcode the parameters.
//   * see "Can you say more about adding a custom annotator?"
//   */
//  class MyWordsToSentencesAnnotator
//    extends WordsToSentencesAnnotator(
//      true,
//      "[.]|[!?]+|[。]|[！？]+",
//      null,
//      null,
//      "never") {
//    // the coreNLP pipeline will call this.
//    // I ignore the properties.
//    def this(name: String, props: Properties) {
//      this()
//    }
//
//  }
//}

/**
 * TODO FNLP不吃阿拉伯數字，還要轉。也不吃英文，分詞器連辨識英文都不會，所以會和中文狗在一起
 */
object FNLP extends DocumentParcer {
  val nerClasses = Set("人名", "地名", "机构名", "型号名", "实体名")
  val NoneNerClass = "O"
  val NounClasses = nerClasses + "名词"

  import org.fnlp.nlp.cn.CNFactory

  lazy val factory: CNFactory = CNFactory.getInstance("./model/FNLP_models")

  override def parseDocument(doc: String, docID: String): Document = {

    val sents = DocumentParcer.split2ChineseSentence(doc)
    val annotatedSentences = sents.map {
      sent => {
        val words = factory.seg(sent)
        val posTags = factory.tag(words)
        //        val nerMap = CNFactory.ner(sent) //.......all had been in pos
        val nerTags = posTags.map(c => if (FNLP.nerClasses.contains(c)) c else FNLP.NoneNerClass)

        //        println("words=" + words.toList)
        //        println("posTags = " + posTags.toList)
        //        println("nerTags = " + nerTags.toList)
        //          println()
        val tokens = (
          for ((word, pos, ner) <- (words, posTags, nerTags).zipped) yield Token(word, pos, ner)
          ).toVector
        Sentence(tokens = tokens)
      }
    }.toVector

    Document(annotatedSentences, Option(docID))

  }

}

