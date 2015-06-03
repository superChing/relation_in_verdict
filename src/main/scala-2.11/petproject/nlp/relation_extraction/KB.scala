package petproject.nlp.relation_extraction

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import play.api.libs.json.{JsValue, Json}

import scala.io.Source
import scala.pickling.Defaults._
import scala.pickling.binary._

/**
 * Author: Wei-Ching Lin
 */


/**
 * Wikidata KB
 * TODO: multiple relations
 * custom relation Json format= {pid: {subject : object}}
 */
object KB {
  val KB_DIR_In_Resource = "/relations"


  /**
   * TODO retrive relation label from WikiData by relation ID ?
   * retrive only one relation class , so you need to specify a relationID or WikiData PID.
   * @param relationID relation id (property id in wikidata)
   * @return
   */
  def getRDF(relationID: Int, relationName: Option[String] = None) = {
    //relations from RDFs
    val kbRelations = Option(getClass.getResource(KB_DIR_In_Resource + s"/$relationID.bin")).map(
      url => RelationDownloader.readBinary(Paths.get(url.toURI))
    ).getOrElse(Map())

    //relations from custom relations in Json
    val myrelation = Some(KB_DIR_In_Resource + "/my_relations.json").map { path =>
      val str = Source.fromURL(getClass.getResource(path)).mkString
      val json = Json.parse(str)
      val m = json.as[Map[String, Map[String, String]]]
      val myrelation = m.getOrElse(relationID.toString, Map()).toSeq.map((_, relationID)).toMap
      myrelation
    }.getOrElse(Map())

    new RelationTupleSet(relationID, kbRelations.keySet ++ myrelation.keySet, relationName)
  }


  /**
   * currently only handle one relation
   * @param entityNamePair
   * @param relationName
   */
  case class RelationTupleSet(relationID: Int, entityNamePair: Set[(String, String)], relationName: Option[String]) {
    /**
     * TODO currently it is strict matching, we need heuristic string similarity matching rule.
     * @param mention1
     * @param mention2
     * @return
     */
    def isRelationCandidate(mention1: String, mention2: String): Boolean =
      entityNamePair((mention1, mention2))

  }

}

//class RelationDownloader(path: Path) 

/**
 * preprocess the KB's dump. the dump are just entity id , we still need to fetch relevant data from web api.
 * @note currently only handle one relation. the data file is assumed to be of one relation only.
 */
object RelationDownloader {
  val language = "zh"


  def saveAsBinary(pair2pid: Map[(String, String), Int], path: Path) = {
    val pkl = pair2pid.pickle
    Files.write(path, pkl.value)
  }
  def readBinary(path: Path) = {
    val bin = Files.readAllBytes(path)
    bin.unpickle[Map[(String, String), Int]]
  }
  def saveAsJson(labelPairs: List[(String, String)], path: String) = {
    val csv = labelPairs.map { case (item1, item2) => s"$item1,$item2\n" }
    Files.write(Paths.get(path), csv.mkString.getBytes(StandardCharsets.UTF_8))
  }
  /**
   * trans it into labels  by web API
   */
  def namePairs(qidPairs: Iterator[(Int, Int)]): Iterator[(String, String)] = {
    val numBatch = 20
    val batch = qidPairs.grouped(numBatch)
    val seqPairs = batch.flatMap {
      tuples => {
        val qids = tuples.flatMap(_.productIterator.map { case e: Int => e })
        //TODO exception handling
        val jsonText = RemoteKB.mkRequest(qids, language).asString.body
        val ls = RemoteKB.parseLabel(qids, jsonText)
        ls.grouped(2).map { case Seq(a, b) => (a, b) }
      }
    }

    def product(arg1s: Seq[String], arg2s: Seq[String]) = for (a1 <- arg1s; a2 <- arg2s) yield (a1, a2)

    seqPairs.flatMap(t => product(t._1, t._2))

  }

  /**
   * WikiData
   */
  object RemoteKB {

    import scalaj.http.Http

    def getQIDPairsFromFile(pid: Int, path: String): Iterator[(Int, Int)] = {
      //property file is a collection of QID pairs
      val propertyFilePath = Paths.get(path)

      /**
       * read Json file into memory
       * the json is {pid1:[ [qid1,qid2],[qid3,qid4],...],pid2:[...],...}
       */
      val json: JsValue = {
        //    val url = Option(getClass.getResource(path))
        //    val source = Source.fromURL(url.getOrElse(throw new FileNotFoundException("the file not found in the
        // classpath")))
        val source = Source.fromFile(propertyFilePath.toFile)
        val string = try source.mkString finally source.close()
        val json = Json.parse(string) \ "props"
        json
      }

      //read Json into Scala
      val pair = """\[(\d+),(\d+)\]""".r
      pair.findAllIn(json.toString())
        .map { case pair(qid1, qid2) => (qid1.toInt, qid2.toInt) }
    }

    /**
     * fetch the labels from Wikidata web API.
     * for each qid get its label using http api.
     * TODO if no value for the selected language,it would backoff to the en language and translate the name.
     * Data in zh is much less than en language
     *
     * @param qids Wikidata item ID
     * @param language  desired language, if not available , fallback to en.
     * @return
     */
    def mkRequest(qids: Seq[Int], language: String) =
      Http("https://www.wikidata.org/w/api.php")
        .params(Seq("action" -> "wbgetentities",
                    "ids" -> qids.map(id => s"Q$id").mkString("|"),
                    "format" -> "json",
                    "props" -> "labels|aliases",
                    "languages" -> s"$language"))


    /**
     * TODO parse all QID, don't depends on qids 
     * @param qids Wikidata item ID
     * @return the optional labels in same order as input
     */
    def parseLabel(qids: Seq[Int], jsonData: String): Seq[Seq[String]] = {
      val json = Json.parse(jsonData)
      val labels = qids.map {
        qid =>
          (json \ "entities" \ s"Q$qid" \\ "value").flatMap(_.asOpt[String])
      }
      labels
    }
  }

}

object preprocessKBDump extends App {
  val language = RelationDownloader.language
  val numFetch = 70000
  val pid = 108

  //  import KB._

  val qidPairs = RelationDownloader.RemoteKB.getQIDPairsFromFile(
    pid,
    "/Users/apple/IdeaProjects/relation_extraction/KB_dump" + s"/${pid}.json"
  )
  //fetch the labels of relation items
  val namePairs = RelationDownloader.namePairs(qidPairs).take(numFetch)
  val relations = namePairs.map((_, pid)).toMap
  //  save the relation to resources
  RelationDownloader
    .saveAsBinary(relations,
                  Paths.get("/Users/apple/IdeaProjects/relation_extraction/src/main/resources/relations" + s"/$pid.bin"))

  //read
  val unpkl = RelationDownloader.readBinary(Paths.get(getClass.getResource(s"/relations/$pid.bin").toURI))
  println("unpkl = " + unpkl.take(100))


}