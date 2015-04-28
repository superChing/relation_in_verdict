package petproject.nlp.relation_extraction

import java.io.FileNotFoundException
import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Files, Paths}

import play.api.libs.json.{JsValue, Json}

import scala.io.Source
import scala.pickling.Defaults._
import scala.pickling.binary._

/**
 * Author: Wei-Ching Lin
 */

/**
 * currently only handle one relation
 * @param relation
 * @param pid
 * @param label
 */
case class RelationExample(relation: Map[(String, String),Int], pid: Int, label: String) {
  /**
   * TODO currently it is strict matching, we need heuristic string similarity matching rule.
   * TODO Wikidata entity alias
   * @param mention1
   * @param mention2
   * @return
   */
  def getRelation(mention1: String, mention2: String): Option[Int ]=
    relation.get((mention1,mention2))
}

/**
 * Wikidata KB
 * TODO: multiple relations
 */
object KB {
  val relationLabel = "employer"
  val pid = 108
  val resourceDir= "/relations"

  def getRelationExamples(pid: Int = pid, extraRelationPath: String =resourceDir+ "/my_relations.json") = {
    val relations = Map(pid -> KBDumpPreprocessor.readBinary(Paths.get(getClass.getResource(resourceDir + s"/$pid.bin").toURI)))
    val relation = relations.getOrElse(pid,Map())

    val str=Source.fromURL(getClass.getResource(extraRelationPath)).mkString
    val json = Json.parse(str)
    val m= json.as[Map[String,Map[String,String]]]
    val myrelation=m.getOrElse("108",Map()).toSeq.map((_,pid)).toMap

    new RelationExample(relation++myrelation, pid, relationLabel)
  }

}

/**
 * preprocess the KB's dump. the dump are just entity id , we still need to fetch data from web api.
 * @note currently only handle one relation. the path is assumed as a dump of one relation.
 *
 * @param path dump's path.
 */
class KBDumpPreprocessor(path: Path) {
  /**
   * read Json file into memory
   * the json is {pid1:[ [qid1,qid2],[qid3,qid4],...],pid2:[...],...}
   */
  lazy val json: JsValue = {
//    val url = Option(getClass.getResource(path))
//    val source = Source.fromURL(url.getOrElse(throw new FileNotFoundException("the file not found in the classpath")))
    val source=Source.fromFile(path.toFile)
    val string = try source.mkString finally source.close()
    val json = Json.parse(string) \ "props"
    json
  }

  //read Json into Scala
  //TODO from here you can improve to multiple relation case
  lazy val qidPairs: Iterator[(Int, Int)] = {
    val pair = """\[(\d+),(\d+)\]""".r
    val qidPairs = pair.findAllIn(json.toString())
      .map { case pair(qid1, qid2) => (qid1.toInt, qid2.toInt) }
    qidPairs
  }


}

object KBDumpPreprocessor {

  val language = "zh"


  def getLabel(qid: Int): Seq[String] = getLabel(Seq(qid), language).head


  /**
   * trans it into labels  by web API
   */
  def namePairs(qidPairs: Iterator[(Int, Int)]): Iterator[(String, String)] = {
    val numBatch = 20
    val batch = qidPairs.grouped(numBatch)
    val seqPairs=batch.flatMap {
      tuples => {
        val seq = tuples.flatMap(_.productIterator.map { case e: Int => e })
        val ls = getLabel(seq)
        ls.grouped(2).map { case Seq(a, b) => (a, b) }
      }
    }

    def product(arg1s:Seq[String],arg2s:Seq[String]):Seq[(String,String)]=for (a1<-arg1s;a2<-arg2s) yield (a1,a2)

    seqPairs.flatMap(t=>product(t._1,t._2))

  }


  /**
   * fetch the labels from Wikidata web API.
   * for each qid get its label using http api.
   * TODO if no value for the selected language,it would backoff to the en language and translate the name.
   * Data in zh is much less than en language
   * @param qids Wikidata item ID
   * @param language  desired language, if not available , fallback to en.
   * @return the optional labels in same order as input
   */
  def getLabel(qids: Seq[Int], language: String = language): Seq[Seq[String]] = {
    import scalaj.http.{Http, HttpResponse}
    val response: HttpResponse[String] =
      Http("https://www.wikidata.org/w/api.php")
        .params(Seq("action" -> "wbgetentities",
                    "ids" -> qids.map(id => s"Q$id").mkString("|"),
                    "format" -> "json",
                    "props" -> "labels|aliases",
                    "languages" -> s"$language"))
        .asString

    val json = Json.parse(response.body)
    val labels = qids.map { qid =>
      (json \ "entities" \ s"Q$qid" \\ "value").flatMap(_.asOpt[String])
    }
    labels
  }

  def saveAsBinary(pair2pid: Map[(String, String),Int], path: Path ) = {
    val pkl = pair2pid.pickle
    Files.write(path, pkl.value)

  }

  def readBinary(path:Path ) = {
    val bin = Files.readAllBytes(path)
    bin.unpickle[Map[(String, String),Int]]
  }

  def saveAsJson(labelPairs: List[(String, String)], path: String) = {
    val csv = labelPairs.map { case (item1, item2) => s"$item1,$item2\n" }
    Files.write(Paths.get(path), csv.mkString.getBytes(StandardCharsets.UTF_8))
  }

}

object preprocessKBDump extends App {
  val language = KBDumpPreprocessor.language
  val numFetch = 70000
  import KB._

  //fetch the labels of relation items
  val preprocessor = new KBDumpPreprocessor(Paths.get("/Users/apple/IdeaProjects/relation_extraction/KB_dump" + s"/$pid.json"))
  val namePairs = KBDumpPreprocessor.namePairs(preprocessor.qidPairs).take(numFetch)
  val relations=namePairs.map((_,pid)).toMap
  //  save the relation to resources
  KBDumpPreprocessor.saveAsBinary(relations, Paths.get("/Users/apple/IdeaProjects/relation_extraction/src/main/resources/relations" + s"/$pid.bin"))

  //read
  val unpkl = KBDumpPreprocessor.readBinary(Paths.get(getClass.getResource(resourceDir + s"/$pid.bin").toURI))
  println("unpkl = " + unpkl.take(100))


}