package petproject.nlp.relation_extraction

import org.apache.spark.mllib.classification.{LogisticRegressionModel, LogisticRegressionWithLBFGS}
import org.apache.spark.mllib.evaluation.MulticlassMetrics
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.util.MLUtils
import org.apache.spark.{SparkConf, SparkContext}
import org.scalatest.FunSuite


/**
 * Author: Wei-Ching Lin
 * Date: 15/4/28
 */

class SparkTest extends FunSuite {

  val master = "local[2]"
  val appName = this.getClass.getSimpleName
  val conf: SparkConf =
    new SparkConf()
      .setMaster(master)
      .setAppName(appName)

  def withSparkContext(test: SparkContext => Any) {
    val sc = new SparkContext(conf)
    try {
      test(sc)
    }
    finally sc.stop()
  }

  test("make RDD") {
    withSparkContext { sc =>
      val lines = Seq("abc def", "123 234")
      val rdd = sc.parallelize(lines)
      val what = rdd.collect()
      what.foreach(println)
    }
  }

  test("logReg") {
    withSparkContext { sc =>
      // Load training data in LIBSVM format.
//      val url=getClass.getResource("sample_libsvm_data.txt")
      val data = MLUtils.loadLibSVMFile(sc, "/Users/apple/IdeaProjects/relation_extraction/src/test/resources/sample_libsvm_data.txt")

      // Split data into training (60%) and test (40%).
      val splits = data.randomSplit(Array(0.6, 0.4), seed = 11L)
      val training = splits(0).cache()
      val test = splits(1)

      // Run training algorithm to build the model
      val model = new LogisticRegressionWithLBFGS()
        .setNumClasses(10)
        .run(training)


      // Compute raw scores on the test set.
      val predictionAndLabels = test.map { case LabeledPoint(label, features) =>
        val prediction = model.predict(features)
        (prediction, label)
      }

      // Get evaluation metrics.
      val metrics = new MulticlassMetrics(predictionAndLabels)
      val precision = metrics.precision
      println("Precision = " + precision)

      // Save and load model
//      model.save(sc, "myModelPath")
//      val sameModel = LogisticRegressionModel.load(sc, "myModelPath")
    }
  }

  //  private var _sc: SparkContext = _
  //  def sc = _sc
  //
  //  override def beforeAll(): Unit = {
  //    super.beforeAll()
  //    _sc = new SparkContext(conf)
  //  }
  //
  //  override def afterAll(): Unit = {
  //    if (_sc != null) {
  //      _sc.stop()
  //      _sc = null
  //    }
  //    super.afterAll()
  //  }

}