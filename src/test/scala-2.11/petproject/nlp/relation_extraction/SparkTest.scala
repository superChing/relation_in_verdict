package petproject.nlp.relation_extraction

import org.apache.spark.mllib.classification.LogisticRegressionWithLBFGS
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