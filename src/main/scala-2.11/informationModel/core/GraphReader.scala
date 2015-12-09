package informationModel.core

import scala.collection.mutable.ArrayBuffer

/**
 * Created by simonshapiro on 09/12/15.
 */
object GraphReader {
  def readFile(filePath: String, dateTimeString: String): graph = {
    val allLines = new ArrayBuffer[String]
    val bufferedSource = scala.io.Source.fromFile(filePath + "/" + dateTimeString + ".dnml")
    for (line <- bufferedSource.getLines()) {
      allLines.append(line)
    }
    bufferedSource.close()
    new graph(allLines.mkString(" "))
  }
}
