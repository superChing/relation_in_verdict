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


  ignore("parseDocument") {
    val text = new ChineseTrans().toSimp("柯文哲受僱於台北市政府。")
    val parser = DocumentParcer.using("stanford")
    val parsed1 = parser.parseDocument(text)
    //    val parsed2 = DocumentParcer.annotateEntityMention(parsed1)
    //    println(parsed1)
    println(Json.prettyPrint(Json.toJson(parsed1)))
  }


  ignore("assignEntityMentsions method infer entity mentions from NER and assign it to the sentence") {
    val sent = Sentence(
      Vector(Token("這裏", null, null, "O"), Token("是", null, null, "O"), Token("台湾", null, null, "GPE"),
             Token("台北市", null, null, "GPE"), Token("。", null, null, "O")),
      null)

    val sent2 = DocumentParcer.annotateEntityMention(sent)
    println(Json.prettyPrint(Json.toJson(sent2)))
    //assert  ... by manual checking
  }

  test("assign entities") {
    val doc = DocumentParcer.annotateEntityMention(DocumentFixture.parsedDoc)
    println(Json.prettyPrint(Json.toJson(doc)))
    val mentions = doc.sentences.flatMap(sent => sent.entityMentions.map(_.mentionString(sent)))
    println("mentions = " + mentions)
  }



  ignore("generate Relations") {
    val docFixture = DocumentFixture.docWithEntityMention
    val doc = DocumentParcer.genCandidateRelation(docFixture)
    //    val doc=doc.sentences.map(sent => sent.relationMentions)
    println(Json.prettyPrint(Json.toJson(doc)))
  }

  test("string to arc") {
    val arcs=Dependency.stanfordString2Arcs(List("root(ROOT-0, 僱于-3)", "nsubj(僱于-3, 柯文哲-1)"))
    assert(arcs.length==2)
    assert(arcs(0)==Arc(0,3,Some("root")))
    assert(arcs(1)==Arc(3,1,Some("nsubj")))

  }
}


object DocumentFixture {

  val parsedDoc =
    Document(Vector(
      Sentence(
        Vector(
          Token("柯文哲", null, "NR", "PERSON"), Token("受", null, "VV", "O"), Token("僱于", null, "VV", "O"),
          Token("台北市", null, "NR", "ORG"), Token("政府", null, "NN", "ORG"), Token("。", null, "PU", "O")),
        List("root(ROOT-0, 僱于-3)", "nsubj(僱于-3, 柯文哲-1)", "mmod(僱于-3, 受-2)", "nn(政府-5, 台北市-4)", "dobj(僱于-3, 政府-5)"),
        Vector(),
        Vector())
    ), None)


  val docWithEntityMention =
    parsedDoc.copy(
      sentences = parsedDoc.sentences.map {
        sent => sent.copy(
          entityMentions = Vector(EntityMention(None, "PERSON", 0, 0), EntityMention(None, "ORG", 3, 4))
        )
      })

  val docWithRelationMentsion =
    parsedDoc.copy(
      sentences = parsedDoc.sentences.map {
        sent => sent.copy(
          relationMentions = Vector(
            RelationMention(None, EntityMention(None, "PERSON", 0, 0), EntityMention(None, "ORG", 3, 4)),
            RelationMention(None, EntityMention(None, "ORG", 3, 4), EntityMention(None, "PERSON", 0, 0)))
        )
      })

  val docWithRelation =
    parsedDoc.copy(
      sentences = parsedDoc.sentences.map {
        sent => sent.copy(
          relationMentions = Vector(
            RelationMention(Some(108), EntityMention(None, "PERSON", 0, 0), EntityMention(None, "ORG", 3, 4)),
            RelationMention(None, EntityMention(None, "ORG", 3, 4), EntityMention(None, "PERSON", 0, 0)))
        )
      })

  //  Document(Vector(
  //    Sentence(
  //      Vector(
  //        Token("柯文哲", null, "NR", "PERSON"), Token("任职", null, "VV", "O"), Token("台湾", null, "NR", "GPE"),
  //        Token("台北市", null, "NR", "GPE"), Token("市长", null, "NN", "O"), Token("。", null, "PU", "O")
  //      ),
  //      List(
  //        "root(ROOT-0, 任职-2)", "nsubj(任职-2, 柯文哲-1)", "nn(台北市-4, 台湾-3)", "nn(市长-5, 台北市-4)", "dobj(任职-2, 市长-5)"),
  //      Vector(
  //        EntityMention(None, "PERSON", 0, 0),
  //        EntityMention(None, "GPE", 2, 3)
  //      ),
  //      Vector(
  //        RelationMention(None, EntityMention(None, "PERSON", 0, 0), EntityMention(None, "GPE", 2, 3)),
  //        RelationMention(None, EntityMention(None, "GPE", 2, 3), EntityMention(None, "PERSON", 0, 0))
  //      )
  //    ),
  //    Sentence(
  //      Vector(Token("马英九", null, "NR", "PERSON"), Token("是", null, "VC", "O"), Token("台湾", null, "NR", "GPE"),
  //             Token("现任", null, "JJ", "O"), Token("总统", null, "NN", "O")),
  //      List("root(ROOT-0, 总统-5)", "nsubj(总统-5, 马英九-1)", "cop(总统-5, 是-2)", "nn(总统-5, 台湾-3)", "amod(总统-5, 现任-4)"),
  //      Vector(
  //        EntityMention(None, "PERSON", 0, 0), EntityMention(None, "GPE", 2, 2)
  //      ),
  //      Vector(
  //        RelationMention(None, EntityMention(None, "PERSON", 0, 0), EntityMention(None, "GPE", 2, 2)),
  //        RelationMention(None, EntityMention(None, "GPE", 2, 2), EntityMention(None, "PERSON", 0, 0))
  //      )
  //    )
  //  ), None)

}