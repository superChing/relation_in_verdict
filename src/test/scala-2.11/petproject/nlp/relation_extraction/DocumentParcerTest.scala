package petproject.nlp.relation_extraction

import org.fnlp.nlp.cn.ChineseTrans
import org.scalatest.FunSuite
import play.api.libs.json.Json

/**
 * Author: Wei-Ching Lin
 */
class DocumentParcerTest extends FunSuite {

  implicit val tFormat = Json.format[Token]
  implicit val eFormat = Json.format[EntityMention]
  implicit val rFormat = Json.format[RelationMention]
  implicit val sFormat = Json.format[Sentence]
  implicit val dFormat = Json.format[Document]

  val text = new ChineseTrans().toSimp("柯文哲受僱於台灣台北市政府，也曾任職於台大醫院。蘋果公司的手機電池要充電。")

  test("sentence splitter") {
    val sents = DocumentParcer.split2ChineseSentence("我我我，我我。我...我，我？!！我。")
    assert(sents sameElements Iterator("我我我，我我。", "我...我，我？!！", "我。"))
  }

  ignore("parseDocument using Stanford NLP") {
    val parser = DocumentParcer.using("stanford")
    val parsed1 = parser.parseDocument(text)
    //    println(parsed1)
    println(Json.prettyPrint(Json.toJson(parsed1)))
  }

  ignore("parseDocument using FNLP") {
    val parser = DocumentParcer.using("FNLP")
    val parsed1 = parser.parseDocument(text)
    println(parsed1)
    //println(Json.prettyPrint(Json.toJson(parsed1)))
  }


  test("infer entity mentions from the parsed chinese sentence") {
    object MockParser extends DocumentParcer {
      override val NoneNerClass = "O"
      override val NounClasses = Set("N")
      override def parseDocument(doc: String, docID: String) = ???
    }

    //LOC+N could be an named entity
    val sent1 = Sentence(Vector(Token("", "A", "O"), Token("台北", "LOC", "LOC"), Token("醫院", "N", "O"), Token("", "C", "O")))
    val res1 = MockParser.annotateChineseEntityMention(sent1).entityMentions
    assert(res1.size == 1)
    assert(res1.head.startIdx == 1)
    assert(res1.head.endIdx == 3)
    assert(res1.head.nerTag == "LOC")
    assert(res1.head.zhString(sent1) == "台北醫院")

    //all N's shoud be not be counted as named entity
    val sent2 = Sentence(Vector(Token("", "A", "O"), Token("", "N", "O"), Token("", "N", "O"), Token("", "C", "O")))
    val res2 = MockParser.annotateChineseEntityMention(sent2).entityMentions
    assert(res2.isEmpty)

    //println(Json.prettyPrint(Json.toJson(res1)))

  }


  test("generate Relations candidates") {
    val ents = Vector(EntityMention(None, "X", 1, 2), EntityMention(None, "Y", 4, 6), EntityMention(None, "Z", 6, 7))
    val sentMock = Sentence(
      Vector(), entityMentions = ents
    )

    val rels = DocumentParcer.genCandidateRelation(sentMock).relationMentions
//    println(Json.prettyPrint(Json.toJson(rels)))

    assert(rels.length == 3)
    assert(rels.contains(RelationMention(None, 0, 1)))
    assert(rels.contains(RelationMention(None, 0, 2)))
    assert(rels.contains(RelationMention(None, 1, 2)))

  }

  //  ignore("string to arc") {
  //    val arcs=Dependency.stanfordString2Arcs(List("root(ROOT-0, touch-3)", "nsubj(touch-3, Tom-1)"))
  //    assert(arcs.length==2)
  //    assert(arcs(0)==Arc(0,3,Some("root")))
  //    assert(arcs(1)==Arc(3,1,Some("nsubj")))
  //  }


}
