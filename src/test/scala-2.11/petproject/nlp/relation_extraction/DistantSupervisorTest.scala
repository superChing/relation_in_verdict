package petproject.nlp.relation_extraction

import org.scalatest.FunSuite

/**
 * Author: Wei-Ching Lin
 * Date: 15/4/26
 */
class DistantSupervisorTest extends FunSuite {

  test("the relationMention should be annotated relation 108"){
    val doc=DocumentFixture.docWithRelationMentsion
    val ds=new DistantSupervisor()
    val annotated=doc.copy(sentences= doc.sentences.map(ds.annotateRelations) )
//    println(annotated)
    assert(annotated.sentences.head.relationMentions.head.label==Some(108))
  }


}
