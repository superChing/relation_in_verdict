package petproject.nlp.relation_extraction

import java.util.Properties

import edu.stanford.nlp.pipeline.WordsToSentencesAnnotator

/**
 * Author: Wei-Ching Lin
 */

/**
 * just a dummy wrapper.
 * Why this?
 * cuz the ssplit annotator of CoreNLP pipeline is not functioning for chinese.
 * I define one myself and hardcode the parameters.
 * see "Can you say more about adding a custom annotator?"
 */
class MyWordsToSentencesAnnotator extends WordsToSentencesAnnotator(
  true,
  "[.]|[!?]+|[。]|[！？]+",
  null,
  null,
  "never") {

  // the coreNLP pipeline will call this.
  // I ignore the properties.
  def this(name: String, props: Properties) {this()}

}
