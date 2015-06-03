package petproject.nlp.relation_extraction

import org.scalatest.FunSuite

/**
 * Author: Wei-Ching Lin
 * Date: 15/4/26
 */
class DistantSupervisorTest extends FunSuite {

  test("the relationMention should be annotated relation -1 using test relation rdf in json file") {
    val sent = Sentence(
      tokens = Vector(Token("Larry", "boom", "boom"),Token("Page", "boom", "boom"),Token("loves","boom","boom"), Token("Marry", "boom", "boom"),Token(".","boom","boom")),
      entityMentions = Vector(EntityMention(None, "boom", 0, 2), EntityMention(None, "boom", 3, 4)),
      relationMentions = Vector(RelationMention(None, 0, 1))
    )

    val rdf=KB.getRDF(-1,None)
    println(rdf)
    val ds = new DistantSupervisor(rdf)
    val annotated = ds.annotateRelations(sent)
    println(annotated.relationMentions)
//    assert(annotated.head.relationMentions.head.label == Some(108))

//    println(getClass.getResource("/relations/my_relations.json"))
  }


}
