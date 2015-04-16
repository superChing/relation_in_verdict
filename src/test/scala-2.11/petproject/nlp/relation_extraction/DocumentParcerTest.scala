package petproject.nlp.relation_extraction

import org.scalatest.FlatSpec

/**
 * Author: Wei-Ching Lin
 */
class DocumentParcerTest extends FlatSpec {


  //  val text = new ChineseTrans().toSimp("柯文哲任職台灣台北市市長。林煒清不是市政府的公務員")
  val sent =
    Sentence(
      "柯文哲任职台湾台北市市长",
      Vector(Token("柯文哲", null, "NR", "PERSON"), Token("任职", null, "VV", "O"), Token("台湾", null, "NR", "GPE"),
        Token("台北市", null, "NR", "GPE"), Token("市长", null, "NN", "O"), Token("。", null, "PU", "O")),
      null
    )

  "assignEntityMentsions method" should "infer entity mentions from NER and assign it to the sentence" in {
    val sent2 = DocumentParcer.annotateEntityMention(sent)
    println(sent2)
    import scala.pickling.Defaults._
    import scala.pickling.json._
    println(sent2.pickle)
    //assert  ... by manual checking
  }


}
