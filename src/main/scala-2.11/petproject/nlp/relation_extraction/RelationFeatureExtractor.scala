package petproject.nlp.relation_extraction

import org.apache.spark.mllib.feature.HashingTF
import org.apache.spark.mllib.regression.LabeledPoint

import scala.collection.mutable.ListBuffer

/**
 * Author: Wei-Ching Lin
 */

object RelationFeatureExtractor {

  val htf = new HashingTF(10000)

  def apply(corpus: Corpus): Iterable[LabeledPoint] = {
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

    val tokensInParentMention = tokens.slice(rel.parent.startIdx, rel.parent.endIdxInclusive)
    val tokensInChildMention = tokens.slice(rel.child.startIdx, rel.child.endIdxInclusive)

    //n-gram words in mentions
    features ++= upToNgram(tokensInParentMention.map(_.word), 2).map(t => s"${t._1}words_in_parent=${t._2}")
    features ++= upToNgram(tokensInChildMention.map(_.word), 2).map(t => s"${t._1}words_in_child=${t._2}")

    //n-gram POS in mentions
    features ++= upToNgram(tokensInParentMention.map(_.posTag), 2).map(t => s"${t._1}posTags_in_parent=${t._2}")
    features ++= upToNgram(tokensInChildMention.map(_.posTag), 2).map(t => s"${t._1}posTags_in_child=${t._2}")

    //n-gram NER in mentions
    features ++= upToNgram(tokensInParentMention.map(_.nerTag), 2).map(t => s"${t._1}nerTags_in_parent=${t._2}")
    features ++= upToNgram(tokensInChildMention.map(_.nerTag), 2).map(t => s"${t._1}nerTags_in_child=${t._2}")

    //word length in mentions
    features += s"word_length_in_parent=${tokensInParentMention.map(_.word).length}"
    features += s"word_length_in_child=${tokensInChildMention.map(_.word).length}"

    //char length in mentions
    features += s"char_length_in_parent=${tokensInParentMention.map(_.word).map(_.length).length}"
    features += s"char_length_in_child=${tokensInChildMention.map(_.word).map(_.length).length}"


    // words , pos,ner around mention
    //filter out null at last
    (1 to 3) foreach { i =>
      //parent
      features += tokens.lift(rel.parent.startIdx - i).map(t => s"word_L${i}_to_parent=${t.word}").orNull
      features += tokens.lift(rel.parent.startIdx - i).map(t => s"pos_L${i}_to_parent= ${t.posTag}").orNull
      features += tokens.lift(rel.parent.startIdx - i).map(t => s"ner_L${i}_to_parent= ${t.nerTag}").orNull
      features += tokens.lift(rel.parent.endIdxInclusive + i).map(t => s"word_R${i}_to_parent=${t.word}").orNull
      features += tokens.lift(rel.parent.endIdxInclusive + i).map(t => s"pos_R${i}_to_parent= ${t.posTag}").orNull
      features += tokens.lift(rel.parent.endIdxInclusive + i).map(t => s"ner_R${i}_to_parent= ${t.nerTag}").orNull

      //child
      features += tokens.lift(rel.child.startIdx - i).map(t => s"word_L${i}_to_child=${t.word}").orNull
      features += tokens.lift(rel.child.startIdx - i).map(t => s"pos_L${i}_to_child= ${t.posTag}").orNull
      features += tokens.lift(rel.child.startIdx - i).map(t => s"ner_L${i}_to_child= ${t.nerTag}").orNull
      features += tokens.lift(rel.child.endIdxInclusive + i).map(t => s"word_R${i}_to_child=${t.word}").orNull
      features += tokens.lift(rel.child.endIdxInclusive + i).map(t => s"pos_R${i}_to_child= ${t.posTag}").orNull
      features += tokens.lift(rel.child.endIdxInclusive + i).map(t => s"ner_R${i}_to_child= ${t.nerTag}").orNull
    }


    //combinations

    //is capital start

    //n-gram between mentions
    val tokensBetween = if (rel.parent.endIdxInclusive > rel.child.startIdx)
                          tokens.slice(rel.parent.endIdxInclusive, rel.child.startIdx)
                        else tokens.slice(rel.child.endIdxInclusive, rel.parent.startIdx)

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
  def upToNgram(seq: Seq[String], n: Int): Iterator[(Int, String)] = {
    val _n = Math.min(n, seq.length)
    _n match {
      case 0 => Iterator.empty
      case _ => seq.sliding(_n).map { window => (_n, window.mkString(" ")) } ++
        upToNgram(seq, _n - 1)
    }
  }


}
