package petproject.nlp

/**
 * Author: Wei-Ching Lin
 */
package object relation_extraction {

  type Corpus = Iterable[Document]

  //if entity[2] and entity[1] becomes a candidate relation, it should, but for the sake of  simplicity.....
  val INVERSE_ENTITY_ORDER = false

  //currently the program only process one relation class .
  val RELATION_ID=108
  val RELATION_LABEL="employer"
}
