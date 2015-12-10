package informationModel.core

import java.nio.file.{Path, Files, Paths}

import play.api.libs.json.Json
import org.joda.time.DateTime
import java.io._


/**
 * Created by simonshapiro on 09/12/15.
 */
object GraphWriter {  // if the graphName does not exist it should be created
  def writeFile(g: graph, graphName: String, filePath: String): Option[String] = {

    val folderPath = Paths.get(filePath + graphName)
    val tmpDir = Files.createDirectories(folderPath)

    val fullFileName = filePath + graphName + "/" + new DateTime().toString

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