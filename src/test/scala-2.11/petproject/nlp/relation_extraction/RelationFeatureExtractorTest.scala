package petproject.nlp.relation_extraction

import org.apache.spark.mllib.linalg.{SparseVector, Vector}
import org.apache.spark.mllib.regression.LabeledPoint
import org.scalatest.FunSuite
import play.api.libs.json.Json

/**
 * Author: Wei-Ching Lin
 * Date: 15/5/6
 */
class RelationFeatureExtractorTest extends FunSuite {

  ignore("test etracting n-gram from string sequence"){
    val res=RelationFeatureExtractor.upToNgram(List("a","b","c"),4)
    assert(res.mkString(",")=="(3,a b c),(2,a b),(2,b c),(1,a),(1,b),(1,c)")
  }

  ignore("get wright feature of the sentnece"){
    val sent=DocumentFixture.docWithRelation.sentences.head
    val rel=sent.relationMentions.head
    val f=RelationFeatureExtractor.extractFeatures(rel, sent)
    println("features="+f)
    println("size="+f.size)
    println("is there duplicate="+(f.toSet.size!=f.size))

//    println("the 2 element that has hash collision="+f.filter(RelationFeatureExtractor.htf.indexOf(_) ==39642))
  }

  ignore("get right LabeledPoint from the corpus"){
    val corpus:Corpus=Seq(DocumentFixture.docWithRelation)
    val res=RelationFeatureExtractor(corpus)
//    implicit val (vFormat ,lFormat)= (Json.format[Vector],Json.format[LabeledPoint])
//    println(Json.prettyPrint(Json.toJson(res.toList)))
    println("point="+res.toList)
    println("size="+ res.toList.head.features.asInstanceOf[SparseVector].indices.length)
  }

}
