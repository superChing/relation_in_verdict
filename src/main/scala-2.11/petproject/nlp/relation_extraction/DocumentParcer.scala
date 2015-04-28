package petproject.nlp.relation_extraction


import java.io.FileNotFoundException
import java.util.Properties

import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.pipeline._
import edu.stanford.nlp.semgraph.SemanticGraph
import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations.CollapsedCCProcessedDependenciesAnnotation
import org.fnlp.nlp.cn.ChineseTrans
import play.api.libs.json.Json

import scala.collection.JavaConversions._


/**
 * Author: Wei-Ching Lin
 */


/**
 * parse the doc string into [[Document]]
 */
trait DocumentParcer {
  def parseDocument(doc: String, docID: String = null): Document

  /**
   * @see [[DocumentParcer.annotateEntityMention(sent: Sentence):Sentence*]]
   * @param doc
   * @return
   */
  def annotateEntityMention(doc: Document): Document = {
    doc.copy(sentences = doc.sentences.map(annotateEntityMention))
  }

  /**
   * @note it do not make entity resolution, the entity is None.
   *       you can distinguish entities by raw mention string, calling EntityMention#mentionString.
   * @param sent sentence that need for finding entities
   * @return sentence with entity mentions
   */
  def annotateEntityMention(sent: Sentence): Sentence = {
    def entityMentions(tokenWithIdx: Seq[(Token, Int)]): Seq[EntityMention] = tokenWithIdx match {
      case seq @ (x, idx) +: xs if x.nerTag != "O" =>
        val segment = seq.takeWhile(_._1.nerTag == x.nerTag)
        val headIdx = segment.head._2
        val endIdx = segment.last._2
        val tag = segment.head._1.nerTag
        EntityMention(None, tag, headIdx, endIdx) +: entityMentions(seq.drop(segment.length))
      case seq @ (x, idx) +: xs if x.nerTag == "O" => entityMentions(xs)
      case Nil => Nil
    }

    sent.copy(entityMentions = entityMentions(sent.tokens.zipWithIndex).toIndexedSeq)
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
      for (e1 <- eWithI; e2 <- eWithI; if e1._2 != e2._2) yield {(e1, e2)}
    }

    val relations = entityPairs(sent.entityMentions).map {
      case ((e1, idx1), (e2, idx2)) => RelationMention(None, e1, e2)
    }
    sent.copy(relationMentions = relations.toIndexedSeq)
  }

}


/**
 * factory object
 */
object DocumentParcer extends DocumentParcer {
  val default: String = "stanford"
  override def parseDocument(doc: String, docID: String): Document = using(default).parseDocument(doc, docID)
  def using(name: String): DocumentParcer = name.toLowerCase match {
    //      case "fnlp" => new FNLP()
    case "stanfordnlp" | "stanford" => new StanfordNLP()
  }

}


class StanfordNLP() extends DocumentParcer {
  val props = new Properties()
  val inputStream = Option(getClass.getClassLoader.getResourceAsStream("StanfordCoreNLP-chinese.properties"))
  props.load(inputStream.getOrElse(throw new FileNotFoundException("property file not found in the classpath")))
  props.put("threads", Runtime.getRuntime.availableProcessors.toString)

  val pipeline = new StanfordCoreNLP(props)

  def parseDocument(doc: String, docID: String = null) = {
    val document = new Annotation(doc)
    pipeline.annotate(document)

    // val docCoref = document.get(classOf[CorefChainAnnotation]) //coreference
    val sentenceAnnos = document.get(classOf[SentencesAnnotation])

    val sentences = sentenceAnnos.zipWithIndex.map {
      case (sentenceAnno, idx) =>
        val tokenAnnos = sentenceAnno.get(classOf[TokensAnnotation])
        val tokens = tokenAnnos.map {
          tokenAnno =>
            val word = tokenAnno.get(classOf[TextAnnotation])
            val lemma = tokenAnno.get(classOf[LemmaAnnotation])
            val pos = tokenAnno.get(classOf[PartOfSpeechAnnotation])
            val ner = tokenAnno.get(classOf[NamedEntityTagAnnotation]) //PERSON, ORGANIZATION, GPE, LOC, MISC, O
            Token(word, lemma, pos, ner)
        }.toVector

        val sentence = sentenceAnno.get(classOf[TextAnnotation])
        val dep = sentenceAnno.get(classOf[CollapsedCCProcessedDependenciesAnnotation])
        Sentence(tokens, dep.toString(SemanticGraph.OutputFormat.LIST).lines.toList)
    }.toVector

    Document(sentences, Option(docID))
  }

}


object StanfordNLP{
  /**
   * just a dummy wrapper.
   * Why this?
   * cuz the ssplit annotator of CoreNLP pipeline is not functioning for chinese.
   * I define one myself and hardcode the parameters.
   * see "Can you say more about adding a custom annotator?"
   */
  class MyWordsToSentencesAnnotator extends WordsToSentencesAnnotator(
    true,
    "[.]|[!?]+|[。]|[！？]+",
    null,
    null,
    "never") {

    // the coreNLP pipeline will call this.
    // I ignore the properties.
    def this(name: String, props: Properties) {
      this()
    }

  }
}


