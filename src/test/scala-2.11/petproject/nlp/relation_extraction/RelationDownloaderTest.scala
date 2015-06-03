package petproject.nlp.relation_extraction

import org.scalatest.{FunSuite, FlatSpec}

/**
 * Author: Wei-Ching Lin
 * Date: 15/4/21
 */
class RelationDownloaderTest extends FunSuite {


  ignore("getLabel fetch corresponding QID labels from Wikidata for the provided QIDs" ) {
    import RelationDownloader.RemoteKB._
    val qids=List(5210392, 49108, 5210392, 13371)
    val req=mkRequest(qids,"zh")
    val labels = parseLabel(qids,req.asString.body)

    println(labels)
  }


  //TODO need `getLabel` mock, don't do testing with internet connection.
  ignore("labelPairs label each qid Tuple2 in length smaller than #batch " ) {
    val labels = RelationDownloader.namePairs(
      List(
        (525169, 49108), (5210392, 13371), (525169, 525169)
      ).iterator)
    assert(labels.length == 3)
  }

  ignore( "labelPairs label each qid Tuple2 in length larger than #batch 20" ) {
    val labels = RelationDownloader.namePairs(List(
      (525169, 49108), (5210392, 13371), (525169, 49108), (5210392, 13371),
      (525169, 49108), (5210392, 13371), (525169, 49108), (5210392, 13371),
      (525169, 49108), (5210392, 13371), (525169, 49108), (5210392, 13371),
      (525169, 49108), (5210392, 13371), (525169, 49108), (5210392, 13371),
      (525169, 49108), (5210392, 13371), (525169, 49108), (5210392, 13371)
    ).iterator)
    assert(labels.length == 21)

  }


}
