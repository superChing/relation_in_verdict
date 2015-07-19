package petproject.nlp.relation_extraction

import org.apache.spark.mllib.feature.HashingTF
import org.apache.spark.mllib.regression.LabeledPoint
import org.fnlp.nlp.cn.ChineseTrans

import scala.collection.mutable.ListBuffer

/**
 * Author: Wei-Ching Lin
 */

/**
 * for relation detector
 */
class SentenceFeatureExtractor(numFeatureHashing:Int=10000) extends FeatureExtractor {
  val htf = new HashingTF(numFeatureHashing)
  def apply(label: Int, sentence: String): LabeledPoint = {
    val ct = new ChineseTrans()
    val zh_sent = ct.toSimp(sentence)
    val parser = DocumentParcer.using("snlpzh")
    val parsed = parser.parseDocument(zh_sent)
    apply(parsed.sentences.head, label)
  }
  def apply(sentence: Sentence, label: Int): LabeledPoint = {
    val features = ListBuffer[String]()
    //    val relationFeatures = sentence.relationMentions.zipWithIndex.map { case (rel, idx) =>
    //      RelationFeatureExtractor.extractFeatures(rel, sentence).map(str => idx + "_" + str)
    //    }
    nGram(features, sentence.tokens, 3)("in_sentence")
    LabeledPoint(label, htf.transform(features))
  }
}

/**
 * feature hashing,
 */
object RelationFeatureExtractor extends FeatureExtractor {

  val htf = new HashingTF(10000)

  def apply(corpus: Iterable[Document]): Iterable[LabeledPoint] = {
    for {
      doc <- corpus
      sent <- doc.sentences
      rel <- sent.relationMentions if rel.label.isDefined
    } yield {
      val features: List[String] = extractFeatures(rel, sent)
      LabeledPoint(rel.label.get.toDouble, htf.transform(features))
    }
  }


  def extractFeatures(rel: RelationMention, sent: Sentence): List[String] = {
    val features = ListBuffer[String]()
    val tokens = sent.tokens

    val parent = rel.getParentEntity(sent)
    val child = rel.getChildEntity(sent)
    val tokensInParentMention = tokens.slice(parent.startIdx, parent.endIdx)
    val tokensInChildMention = tokens.slice(child.startIdx, child.endIdx)

    //TODO pass buffer in for speed
    features ++= featuresInMention(tokensInParentMention)("in_parent")
    features ++= featuresInMention(tokensInChildMention)("in_child")


    featureAroundMention(features, parent, sent)("parent")
    featureAroundMention(features, child, sent)("child")


    //combinations

    //is capital start

    //n-gram between mentions
    val tokensBetween = if (parent.endIdx < child.startIdx)
                          tokens.slice(parent.endIdx, child.startIdx)
                        else
                          tokens.slice(child.endIdx, parent.startIdx)


    nGram(features, tokensBetween, 3)("between_mentions")

    //lemma between mentions
    val words_between = tokensBetween.map(_.word)
    //word length between mentions
    features += s"word_length_between_mentions=${words_between.length}"
    //char length between
    features += s"char_length_between_mentions=${words_between.map(_.length).sum}"

    //shortest dependency path between mentions

    //keyword dictionary

    //TODO get rid the null
    features.filterNot(_ == null).toList
  }

  def featureAroundMention(features: ListBuffer[String], entity: EntityMention, sent: Sentence)
                          (tag: String): Unit = {
    val tokens = sent.tokens
    // words , pos,ner around mention
    //filter out null at last
    (1 to 3) foreach { i =>
      features += tokens.lift(entity.startIdx - i).map(t => s"word_L${i}_to_${tag}=${t.word}").orNull
      features += tokens.lift(entity.startIdx - i).map(t => s"pos_L${i}_to_${tag}= ${t.posTag}").orNull
      features += tokens.lift(entity.startIdx - i).map(t => s"ner_L${i}_to_${tag}= ${t.nerTag}").orNull
      features += tokens.lift(entity.endIdx - 1 + i).map(t => s"word_R${i}_to_${tag}=${t.word}").orNull
      features += tokens.lift(entity.endIdx - 1 + i).map(t => s"pos_R${i}_to_${tag}= ${t.posTag}").orNull
      features += tokens.lift(entity.endIdx - 1 + i).map(t => s"ner_R${i}_to_${tag}= ${t.nerTag}").orNull
    }

  }

  def featuresInMention(tokens: IndexedSeq[Token])(tag: String): ListBuffer[String] = {
    val features = ListBuffer[String]()

    nGram(features, tokens, 2)(tag)

    //length
    //word length in mentions
    features += s"word_length_in_${tag}=${tokens.map(_.word).length}"
    //char length in mentions
    features += s"char_length_in_${tag}=${tokens.map(_.word.length).sum}"

  }


}

trait FeatureExtractor {

  def nGram(features: ListBuffer[String], tokens: IndexedSeq[Token], n: Int)(tag: String): Unit = {
    //N-Gram
    //n-gram words
    features ++= upToNgram(tokens.map(_.word), n).map(t => s"${t._1}words_${tag}=${t._2}")
    //n-gram POS
    features ++= upToNgram(tokens.map(_.posTag), n).map(t => s"${t._1}posTags_${tag}=${t._2}")
    //n-gram NER
    features ++= upToNgram(tokens.map(_.nerTag), n).map(t => s"${t._1}nerTags_${tag}=${t._2}")

  }

  def upToNgram(seq: Seq[String], n: Int): Iterator[(Int, String)] = {
    val _n = Math.min(n, seq.length)
    _n match {
      case 0 => Iterator.empty
      case _ => seq.sliding(_n).map { window => (_n, window.mkString(" ")) } ++ upToNgram(seq, _n - 1)
    }
  }

}
