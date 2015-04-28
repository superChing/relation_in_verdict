package petproject.nlp.relation_extraction

import org.scalatest.FlatSpec

/**
 * Author: Wei-Ching Lin
 * Date: 15/4/21
 */
class KBDumpPreprocessorTest extends FlatSpec {

  "getLabel" should "fetch corresponding labels from Wikidata for the provided QIDs" ignore {
    val labels = KBDumpPreprocessor.getLabel(List(5210392, 49108, 5210392, 13371))
    println(labels)
  }


  //TODO need `getLabel` mock, don't do testing with internet connection.
  "labelPairs" should "label each qid Tuple2 in length smaller than #batch " in {
    val labels = KBDumpPreprocessor.namePairs(
      List(
        (525169, 49108), (5210392, 13371), (525169, 525169)
      ).iterator)
    assert(labels.length == 3)
  }

  it should s"label each qid Tuple2 in length larger than #batch 20" in {
    val labels = KBDumpPreprocessor.namePairs(List(
      (525169, 49108), (5210392, 13371), (525169, 49108), (5210392, 13371),
      (525169, 49108), (5210392, 13371), (525169, 49108), (5210392, 13371),
      (525169, 49108), (5210392, 13371), (525169, 49108), (5210392, 13371),
      (525169, 49108), (5210392, 13371), (525169, 49108), (5210392, 13371),
      (525169, 49108), (5210392, 13371), (525169, 49108), (5210392, 13371)
    ).iterator)
    assert(labels.length == 21)

  }


}
