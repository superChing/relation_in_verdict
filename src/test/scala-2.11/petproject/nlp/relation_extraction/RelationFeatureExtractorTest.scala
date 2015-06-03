package petproject.nlp.relation_extraction

import org.apache.spark.mllib.linalg.SparseVector
import org.scalatest.FunSuite

import scala.collection.mutable.ListBuffer

/**
 * Author: Wei-Ching Lin
 */
class RelationFeatureExtractorTest extends FunSuite {

  test("test etracting n-gram from string sequence") {
    val res = RelationFeatureExtractor.upToNgram(List("a", "b", "c"), 4)
    assert(res.mkString(",") == "(3,a b c),(2,a b),(2,b c),(1,a),(1,b),(1,c)")
  }

  val sent = Sentence(
    tokens = Vector(
      Token("Larry", "PN", "PERSON"), Token("Page", "PN", "PERSON"),
      Token("loves", "V", "O"), Token("Marry", "PN", "PERSON"),
      Token(",", "PUNCT", "O"), Token("not", "ADB", "O"), Token("Jean", "PN", "PERSON")),
    entityMentions = Vector(
      EntityMention(None, "PERSON", 0, 2), EntityMention(None, "PERSON", 3, 4), EntityMention(None, "PERSON", 6, 7)),
    relationMentions = Vector(RelationMention(Some(-1), 0, 1))
  )
  val rel = sent.relationMentions.head
  val parent = rel.getParentEntity(sent)
  val child = rel.getChildEntity(sent)

  test("get the right features in mention ") {
    val tokensInParentMention = sent.tokens.slice(parent.startIdx, parent.endIdx)
    val feature = RelationFeatureExtractor.featuresInMention(tokensInParentMention)("test")
    //    println(s"features =\n ${feature.mkString("\n")}")
    //    println("size = " + feature.size)
    assert(feature.size == 11)
    assert(feature(0) matches ".*=Larry Page")
    assert(feature(2) matches ".*=Page")
    assert(feature(6) matches ".*=PERSON PERSON")
    assert(feature(9) matches ".*=2")
    assert(feature(10) matches ".*=9")

  }


  test("get the right features around mention") {
    val featuresBuffer = ListBuffer[String]()
    RelationFeatureExtractor.featureAroundMention(featuresBuffer, parent, sent)("test")
    val features = featuresBuffer.toList

//    println(s"features =\n ${features.mkString("\n")}")
//    println("size = " + features.size)
        assert(features.count(_ != null) == 9)
  }

  test("get right features"){
    val features=RelationFeatureExtractor.extractFeatures(rel,sent)
//    println(s"features =\n ${features.mkString("\n")}")
//        println("size = " + features.size)

    assert(features.size == 48)

  }

    test("get right LabeledPoint from the corpus"){
      val corpus:Corpus=Seq(Document(Vector(sent,sent)))
      val points=RelationFeatureExtractor(corpus).toList
//      println("point="+res.toList)
//      println("size="+ res.toList.head.features.asInstanceOf[SparseVector].indices.length)

      assert(points(0) == points(1))
      assert(points(0).label == -1)
    }

}
