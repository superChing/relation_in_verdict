package petproject.nlp.relation_extraction

import java.io.{OutputStream, PrintStream}

import org.fnlp.nlp.cn.ChineseTrans

/**
 * Author: Watson Lin
 * Date: 15/4/11
 */
object Main extends App {
  //  val doc=verdict_text_preprocessor.extract_lines(Source.fromFile("/Users/apple/verdict/TaoyuanLo_20140817_105544
  // .csv").getLines()).mkString
  //  val docs=Iterator(doc)
  //  val docs = Iterator("I hate you. I like you.","How are you today? Good!")
  val parser = DocumentParcer.using("FNLP")
  val ct = new ChineseTrans()
  val ds = new DistantSupervisor(KB.getRDF(108, None))


  //StanfordNLP太吵了
//  val original = System.err
//  System.setErr(new PrintStream(new OutputStream() {
//    override def write(b: Int) { /*DO NOTHING        */ }
//  }))

  val docs = verdict_text_preprocessor.extractMainText()
  val parsedDocs = docs
    .map(ct.toSimp)
    .map(parser.parseDocument(_))
    .map(parser.annotateChineseEntityMention)
    .map(DocumentParcer.genCandidateRelation)
    .map(ds.annotateRelations)
    .map(x => {println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"); x})

  //  parsedDocs.map(_.sentences.map(_.relationMentions.count(_.label.isDefined)).sum ).sum

  val n_label = (for {doc <- parsedDocs.take(5)
                      sent <- doc.sentences
                      r <- sent.relationMentions
                      l <- r.label} yield {println(s"catch it: ${r.getParentEntity(sent)}-${r.getChildEntity(sent)}"); 1}).sum
  //
  println("the #hit for relation supervision is " + n_label)

//  System.setErr(original)

  //  RelationFeatureExtractor(parsed.toIterable)

}
