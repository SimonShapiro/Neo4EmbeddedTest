package informationModel.core

import informationModel.dsl.modelDsl

import scala.collection.mutable.ArrayBuffer

/**
 * Created by simonshapiro on 09/12/15.
 */
object GraphReader {
  def getLinesFromFile(gName: String, dateTimeString: String, path: String): ArrayBuffer[String] = {
    val allLines = new ArrayBuffer[String]
    val bufferedSource = scala.io.Source.fromFile(path + gName + "/" + dateTimeString + ".dnml")
    for (line <- bufferedSource.getLines()) {
      allLines.append(line)
    }
    bufferedSource.close()
    allLines
  }
  def readFile(gName: String, dateTimeString: String, path: String): graph = {
    val allLines = getLinesFromFile(gName: String, dateTimeString: String, path: String)
    new graph(allLines.mkString(" "),modelDsl)
  }
  def readFileWithDsl(gName: String, dateTimeString: String, path: String, dsl: Dsl): graph = {
    val allLines = getLinesFromFile(gName: String, dateTimeString: String, path: String)
    new graph(allLines.mkString(" "),dsl)
  }
}
