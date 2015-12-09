package informationModel.core

import org.joda.time.DateTime
import java.io._

import play.api.libs.json.Json

/**
 * Created by simonshapiro on 09/12/15.
 */
object GraphWriter {
  def writeFile(g: graph, filePath: String): Option[String] = {
    val fullFileName = filePath + "/" + new DateTime().toString
    val gString = Json.prettyPrint(Json.parse(g.toJsonAsDyNetML))
    val dir = new File(filePath)
    try {
      val pw = new PrintWriter(new File(fullFileName + ".dnml"))
      pw.print(gString)
      pw.close
      Some(fullFileName)
    }
    catch {
      case e: FileNotFoundException => {
        println(e)
        None
      }
    }
  }
}

/*
// PrintWriter
pw.write("Hello, world")
pw.close

// FileWriter
val bw = new BufferedWriter(new FileWriter(file))
bw.write(text)
bw.close()

*/