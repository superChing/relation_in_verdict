package petproject.nlp.relation_extraction

import org.apache.spark.mllib.feature.HashingTF
import org.apache.spark.mllib.regression.LabeledPoint

import scala.collection.mutable.ListBuffer

/**
 * Author: Wei-Ching Lin
 */

object RelationFeatureExtractor {

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
    features ++= featuresInMention(tokensInParentMention)("parent")
    features ++= featuresInMention(tokensInChildMention)("child")


    featureAroundMention(features,parent,sent)("parent")
    featureAroundMention(features,child,sent)("child")


    //combinations

    //is capital start

    //n-gram between mentions
    val tokensBetween = if (parent.endIdx < child.startIdx)
                          tokens.slice(parent.endIdx, child.startIdx)
                        else
                          tokens.slice(child.endIdx, parent.startIdx)

    val words_between = tokensBetween.map(_.word)
    features ++= upToNgram(tokensBetween.map(_.word), 3).map(t => s"${t._1}words_between=${t._2}")

    val posTags_between = tokensBetween.map(_.posTag)
    features ++= upToNgram(posTags_between, 3).map(t => s"${t._1}posTags_between=${t._2}")

    val nerTags_between = tokensBetween.map(_.nerTag)
    features ++= upToNgram(nerTags_between, 3).map(t => s"${t._1}nerTags_between=${t._2}")

    //lemma between mentions

    //word length between mentions
    features += s"word_length_between=${words_between.length}"

    //char length between
    features += s"char_length_between=${words_between.map(_.length).sum}"

    //shortest dependency path between mentions

    //keyword dictionary

    features.filterNot(_ == null).toList
  }

  def featureAroundMention(features: ListBuffer[String],entity:EntityMention,sent:Sentence)(tag:String): ListBuffer[String] = {
    val tokens=sent.tokens
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

    features
  }

  def featuresInMention(tokensInMention: IndexedSeq[Token])(tag: String): ListBuffer[String] = {
    val features = ListBuffer[String]()

    //N-Gram
    //n-gram words in mentions
    features ++= upToNgram(tokensInMention.map(_.word), 2).map(t => s"${t._1}words_in_${tag}=${t._2}")
    //n-gram POS in mentions
    features ++= upToNgram(tokensInMention.map(_.posTag), 2).map(t => s"${t._1}posTags_in_${tag}=${t._2}")
    //n-gram NER in mentions
    features ++= upToNgram(tokensInMention.map(_.nerTag), 2).map(t => s"${t._1}nerTags_in_${tag}=${t._2}")

    //length
    //word length in mentions
    features += s"word_length_in_${tag}=${tokensInMention.map(_.word).length}"
    //char length in mentions
    features += s"char_length_in_${tag}=${tokensInMention.map(_.word.length).sum}"

  }
  def upToNgram(seq: Seq[String], n: Int): Iterator[(Int, String)] = {
    val _n = Math.min(n, seq.length)
    _n match {
      case 0 => Iterator.empty
      case _ => seq.sliding(_n).map { window => (_n, window.mkString(" ")) } ++
                upToNgram(seq, _n - 1)
    }
  }


}
