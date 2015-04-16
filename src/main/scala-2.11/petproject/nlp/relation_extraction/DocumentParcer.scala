package petproject.nlp.relation_extraction


import java.io.FileNotFoundException
import java.util.Properties

import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.pipeline._
import edu.stanford.nlp.semgraph.SemanticGraph
import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations.CollapsedCCProcessedDependenciesAnnotation
import org.fnlp.nlp.cn.ChineseTrans

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
   * @note it do not make entity resolution,  just distinguish entities by raw mention string.
   * @param sent sentence that need for finding entities
   * @return sentence with entity mentions
   */
  def annotateEntityMention(sent: Sentence):Sentence = {
    def entityMentions(tokenWithIdx: Seq[(Token, Int)]): Seq[EntityMention] = tokenWithIdx match {
      case seq @ (x, idx) +: xs if x.nerTag != "O" =>
        val segment = seq.takeWhile(_._1.nerTag == x.nerTag)
        val words = segment.map(_._1.word).mkString("")
        val headIdx = segment.head._2
        val endIdx = segment.last._2
        val tag = segment.head._1.nerTag
        EntityMention(Option(words),tag, headIdx, endIdx ) +: entityMentions(seq.drop(segment.length))
      case seq @ (x, idx) +: xs if x.nerTag == "O" => entityMentions(xs)
      case Nil => Nil
    }

    sent.copy(entityMentions = entityMentions(sent.tokens.zipWithIndex).toIndexedSeq)
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
  val inputStream = getClass.getClassLoader.getResourceAsStream("StanfordCoreNLP-chinese.properties")
  if (inputStream != null) props.load(inputStream)
  else throw new FileNotFoundException("property file not found in the classpath")
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
        Sentence(sentence, tokens,
          dep.toString(SemanticGraph.OutputFormat.LIST).lines.toList)
    }.toVector

    Document(sentences, Option(docID))
  }

}



object Test extends App {
  val text = new ChineseTrans().toSimp("柯文哲任職台灣台北市市長。林煒清不是市政府的公務員")
  val parser = DocumentParcer.using("stanford")
  val parsed1 = parser.parseDocument(text)
  val parsed2 = parser.annotateEntityMention(parsed1)


  println(parsed2)


}