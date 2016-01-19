package informationModel.core

import java.nio.file.{Path, Files, Paths}
import scala.collection.mutable
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

    val fullFileName = filePath + graphName + "/" + new DateTime().toString.replace(":","_")

    val gString = Json.prettyPrint(Json.parse(g.toJsonAsDyNetML.toString))
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

  def writeDyNetMl(g: graph, graphName: String, filePath: String, iconMap: (String) => String) = {
    def unique[A](ls: List[A]) = {
      def loop(set: Set[A], ls: List[A]): List[A] = ls match {
        case hd :: tail if set contains hd => loop(set, tail)
        case hd :: tail => hd :: loop(set + hd, tail)
        case Nil => Nil
      }
      loop(Set(), ls)
    }

    val x =
      <DynamicMetaNetwork id={graphName}>
        <MetaNetwork id={graphName}>
          <Nodes>
            {unique[String](g.nodes.map(n => n._2.getType).toList).map(nn =>
            <nodeclass id={nn} type={iconMap(nn)}>
              {g.nodes.filter(nodes => nodes._2._type == nn).map(node =>
              <node id={node._2.id}>
                {node._2.memberProperties.map(prop =>
                  <property id={prop._1} type={prop._2._1} value={prop._2._2}/>
              )}
              </node>
            )}
            </nodeclass>)}
          </Nodes>
          <Networks>
            {unique[String](g.edges.map(n => n._2.getType).toList).map(ee =>
            <network sourceType={iconMap(ee.split('_')(0))} source={ee.split('_')(0)} targetType={iconMap(ee.split('_')(2))} target={ee.split('_')(2)} id={ee} isDirected="true" allowSelfLoops="true" isBinary="false" dynetmlUndirectedCompressed="false">
              {g.edges.filter(edges => edges._2._type == ee).map(edge =>
              <link source={edge._2.from.id} target={edge._2.to.id}>
                {edge._2.memberProperties.map(prop =>
                  <property id={prop._1} type={prop._2._1} value={prop._2._2}/>
              )}
              </link>
            )}
            </network>
          )}
          </Networks>
        </MetaNetwork>
      </DynamicMetaNetwork>

    val folderPath = Paths.get(filePath + graphName)
    val tmpDir = Files.createDirectories(folderPath)
    val fullFileName = filePath + graphName + "/" + new DateTime().toString.replace(":","_")
    val xmlStr = x.toString
    try {
      val pw = new PrintWriter(new File(fullFileName + ".xml"))
      pw.print(xmlStr)
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