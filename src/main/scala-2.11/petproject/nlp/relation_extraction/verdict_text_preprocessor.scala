package petproject.nlp.relation_extraction

/**
 * Author: Wei-Ching Lin
 */

import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source

object verdict_text_preprocessor {
  val keywords = Set( """\s*主\s*文\s*""",
                      """\s*理\s*由\s*""",
                      """\s*事\s*實\s*""",
                      """\s*犯\s*罪\s*事\s*實\s*""")
  val stopwords = Set( """\s*中\s*華\s*民\s*國.*年.*月.*日\s*""")
  val skipwords = Set("┌", "│", "├", "└")

  /**
   * @return file name, string (whole text)
   */
  def extractMainText(): Iterator[(String, String)] = {
    val files = new File("/Users/apple/verdict").listFiles.filter(_.getName.contains("csv"))
    val file$text = files.toIterator.map {
      file => (file.getName ,extract_maintext(Source.fromFile(file).getLines()).mkString + "\n")
    }
    file$text
  }
  def extractMainText(file:String): String = {
    extract_maintext(Source.fromFile(file).getLines()).mkString + "\n"
  }

  private def extract_maintext(lines: Iterator[String]): Iterator[String] =
    lines.map(_.trim)
      .dropWhile(l => !keywords.exists(l.matches))
      .takeWhile(l => !stopwords.exists(l.matches))
      .filter(l => !keywords.exists(l.matches))
      .filter(l => !skipwords.exists(l.contains))

  def save(lines: Iterator[String]) = {
    val file = new File("/Users/apple/all_verdicts.txt")
    val bw = new BufferedWriter(new FileWriter(file))
    lines.foreach(bw.append)
    bw.close()
  }
  //    println(extract_maintext(Source.fromFile("/Users/apple/verdict/TaoyuanLo_20140817_105544.csv").getLines()).mkString)
  //    println(lines.take(2).mkString)
  //  val test = Iterator("abc", " 主  文 ", "行1, 主文 。", "【附表甲】┌─┬───────"," 事  實 ", "行2", "中 華 民 國1年 2月3日 ")
  //  println(extract_maintext(test).toList)

}