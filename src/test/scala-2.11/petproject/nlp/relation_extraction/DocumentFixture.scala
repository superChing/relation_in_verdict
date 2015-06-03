package petproject.nlp.relation_extraction

/**
 * Author: Wei-Ching Lin
 */


object DocumentFixture {
}

//  val parsedDoc =
//    Document(Vector(
//      Sentence(
//        Vector(
//          Token("柯文哲", "NR", "PERSON"), Token("受", "VV", "O"), Token("僱于", "VV", "O"),
//          Token("台北市", "NR", "ORG"), Token("政府", "NN", "ORG"), Token("。", "PU", "O")),
//        Some(List("root(ROOT-0, 僱于-3)", "nsubj(僱于-3, 柯文哲-1)", "mmod(僱于-3, 受-2)", "nn(政府-5, 台北市-4)", "dobj(僱于-3, 政府-5)")),
//        Vector(),
//        Vector())
//    ), None)


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

//}