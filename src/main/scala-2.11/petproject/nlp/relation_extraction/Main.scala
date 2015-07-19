package petproject.nlp.relation_extraction

import org.apache.spark.mllib.classification.LogisticRegressionWithLBFGS
import org.apache.spark.mllib.evaluation.MulticlassMetrics
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.{SparkConf, SparkContext}
import org.fnlp.nlp.cn.ChineseTrans

import scala.io.Source

//subsentence level (comma separated sentence) relation detection
object RelationDetectionMain extends App {


  //  StanfordNLP太吵了
  //  val original = System.err
  //  System.setErr(new PrintStream(new OutputStream() {
  //    override def write(b: Int) { /*DO NOTHING        */ }
  //  }))

  //extra negative samples generated by filtering out positives and take the rest subsentences
  val positives = Source
    .fromFile("/Users/apple/IdeaProjects/relation_extraction/trainingset1.txt")
    .getLines()    .map(_.trim)    .toSet
  val extraNegatives =(len:Int)=> DocumentParcer.split2ChineseSentence(
    verdict_text_preprocessor.extractMainText(
      "/Users/apple/IdeaProjects/relation_extraction/ChanghuaLo_20140817_115636.csv"), len)
    .map(_.trim().stripSuffix("，").stripSuffix("。")).filterNot(n=>positives.exists(p=>p.contains(n)||n.contains(p)))
  //  println(extraNegatives.toList)

  val fe = new SentenceFeatureExtractor(1000)
  val toPoints = (file: String, label: Int) => Source.fromFile(file).getLines().map(_.trim).map(line => fe(label, line))
  val points = {
    toPoints("/Users/apple/IdeaProjects/relation_extraction/trainingset1.txt", 1) ++
    toPoints("/Users/apple/IdeaProjects/relation_extraction/trainingset0.txt", 0) ++
    extraNegatives(1).map(line => fe(0, line)) ++
    extraNegatives(2).map(line => fe(0, line))
  }.toSeq


  println("training size = " + points.size)

  //  val points = Source.fromFile("").getLines().map(_.trim).map { line =>
  //    val splits = line.split("\b", 2)
  //    val label = splits(0).toInt
  //    val sentence = splits(1)
  //    SentenceFeatureExtractor(label, sentence)
  //  }
  val conf: SparkConf = new SparkConf().setMaster("local[2]").setAppName(this.getClass.getSimpleName)
  val sc = new SparkContext(conf)
  try logReg(sc, points.toSeq)
  finally sc.stop()

  def logReg(sc: SparkContext, data: Seq[LabeledPoint]) {
    //      val url=getClass.getResource("sample_libsvm_data.txt")
    val parallel_data = sc.parallelize(data)

    // Split data into training (60%) and test (40%).
    val splits = parallel_data.randomSplit(Array(0.6, 0.4), seed = 11L)
    val training = splits(0).cache()
    val test = splits(1)

    // Run training algorithm to build the model
    val model = new LogisticRegressionWithLBFGS()
      .setNumClasses(2)
      .run(training)

    // Get evaluation metrics.
    val predictionAndLabels = test.map { case LabeledPoint(label, features) => (model.predict(features), label) }
    val metrics = new MulticlassMetrics(predictionAndLabels)
    val precision = metrics.precision(1)
    println("Precision = " + precision)
    val recall = metrics.recall(1)
    println("recall = " + recall)

    // Save and load model
    //      model.save(sc, "myModelPath")
    //      val sameModel = LogisticRegressionModel.load(sc, "myModelPath")

  }


}

/**
 * Author: Watson Lin
 * Date: 15/4/11
 */
object RelationExtractionMain extends App {
  //  val doc=verdict_text_preprocessor.extract_lines(Source.fromFile("/Users/apple/verdict/TaoyuanLo_20140817_105544
  // .csv").getLines()).mkString
  //  val docs=Iterator(doc)
  //  val docs = Iterator("I hate you. I like you.","How are you today? Good!")
  val parser = DocumentParcer.using("snlpzh")
  val ct = new ChineseTrans()
  val ds = new DistantSupervisor(KB.getRDF(108, None))


  //  StanfordNLP太吵了
  //  val original = System.err
  //  System.setErr(new PrintStream(new OutputStream() {
  //    override def write(b: Int) { /*DO NOTHING        */ }
  //  }))

  val fName$docs = verdict_text_preprocessor.extractMainText()
  val parsedDocs = fName$docs.map { case (fName, doc) =>
    val szh_doc = ct.toSimp(doc)
    val parsed1 = parser.parseDocument(szh_doc)
    val parsed2 = parser.annotateChineseEntityMention(parsed1)
    val parsed3 = DocumentParcer.genCandidateRelation(parsed2)
    val parsed4 = ds.annotateRelations(parsed3)
    println("XXXXXXXXXXXX a doc is just processed XXXXXXXXXXXXXXXXXX")
    println(parsed4)
    parsed4
  }

  val n_hits = parsedDocs.take(1).map {
    _.sentences.map { sent =>
      sent.relationMentions.map { r =>
        if (r.label.isDefined) {
          println(s"catch: ${r.getParentEntity(sent)}-${r.getChildEntity(sent)}")
          1
        } else 0
      }.sum
    }.sum
  }.sum

  println("the #hit for relation supervision is " + n_hits)

  //  System.setErr(original)

  //  RelationFeatureExtractor(parsed.toIterable)

}
