package informationModel.core

import java.io.{StringWriter, File}
import java.util

import freemarker.template.{SimpleHash, TemplateExceptionHandler, Configuration}
import informationModel.kernel.{MetaNode, Property, MetaEdgeNode}
import play.api.libs.json.{JsPath, Reads}
import play.api.libs.functional.syntax._

import scala.collection.JavaConversions._

/**
 * Created by simonshapiro on 15/12/15.
 */
object DslGenerator {

  val cfg = new Configuration(Configuration.VERSION_2_3_23)
  cfg.setDirectoryForTemplateLoading(new File("/Users/simonshapiro/IdeaProjects/Neo4EmbeddedTest/src/main/scala-2.11/informationModel/kernel/templates"))
  cfg.setDefaultEncoding("UTF-8")
  cfg.setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER)

  def generateDsl(templateFile: String, g: graph) = {
    val template = cfg.getTemplate(templateFile)
    // val out = new OutputStreamWriter(System.out)
    val out = new StringWriter
    val data = new java.util.HashMap[String, Object]  // to be replaced
    val nodes = new java.util.ArrayList[node]
/*
    val gInternal = g.toJsonAsDyNetML

    implicit  val propReads: Reads[propJson] = (
      (JsPath \ "name").read[String] and  // readNullable?
      (JsPath \ "type").read[String] and
      (JsPath \ "value").read[String]
      )(propJson.apply _)

    implicit val nodeReads: Reads[nodeJson] = (
      (JsPath \ "id").read[String]   and // readNullable?
      (JsPath \ "$type").read[String] and
      (JsPath \ "properties").read[List[propJson]]
    )(nodeJson.apply _)

    implicit val edgeReads: Reads[edgeJson] = (
      (JsPath \ "id").read[String]   and // readNullable?
      (JsPath \ "$type").read[String] and
      (JsPath \ "from").read[String] and
      (JsPath \ "to").read[String] and
      (JsPath \ "properties").read[List[propJson]]
    )(edgeJson.apply _)

    val nodeRes = (gInternal \ "graph" \ "nodes").as[List[nodeJson]]
    val edgeRes = (gInternal \ "graph" \ "edges").as[List[edgeJson]]
*/
    class propJava(val name: String, val valueType: String)
    class nodeJava(val id: String, val _type: String, val propertiesJava: java.util.Vector[propJava])
    class edgeJava(val id: String, val _type: String, val from: String, val to: String, val propertiesJava: java.util.Vector[propJava])

//    g.nodes.foreach(n => nodes += n._2)
    val nodeJavaMap = new java.util.Vector[nodeJava]()
    g.nodes.filter(metaNode => (metaNode._2.isOfType("MetaNode"))).foreach(n => {  // find properties from HASPROPERTIES edge
            val props = new util.Vector[propJava]()
            val propTarget = g.edges.filter(e => ((e._2.isOfType("MetaNodeHASPROPERTIESProperty")) && (n._2.asInstanceOf[MetaNode].id == e._2.from.id)))
            propTarget.map(t => g.getNode(t._2.to.id).asInstanceOf[Property]).foreach(p => {
              props += new propJava(p.name.getOrElse(p.id), p.valueType.getOrElse("String"))
            })
            val nodeJ = new nodeJava(n._2.id, n._2.getType, props)
            nodeJavaMap += nodeJ
    })
    data("nodes") = nodeJavaMap

    val edgeJavaMap = new java.util.Vector[edgeJava]()
    val edgesList = g.edges.filter(e => (e._2.getType.contains("CONNECTS"))).foreach(e => {  // find association class that is also a MetaEdge
            val props = new util.Vector[propJava]()
            e._2.associatedWith match {
              case Some(x) =>   if (x.isInstanceOf[MetaEdgeNode]) {
                                  val propTarget = g.edges.filter(e => ((e._2.isOfType("MetaEdgeHASPROPERTIESProperty")) && (x.id == e._2.from.id)))
                                  propTarget.map(t => g.getNode(t._2.to.id).asInstanceOf[Property]).foreach(p => {
                                    props += new propJava(p.name.getOrElse(p.id), p.valueType.getOrElse("String"))
                                  })
//                                  println("Has ",x)
                                }
                                else {
                                  props += new propJava(x.id, "AssociationClass")
                                  println("Has association class")
                                }
              case None    =>  // println("Doesn't")
            }
            val edgeJ = new edgeJava(e._2.id,e._2.getType,e._2.from.id,e._2.to.id,props)
            edgeJavaMap += edgeJ
    })
    val triplesList = edgesList

    data("edges") = edgeJavaMap

    //    val dataAsJava = asJavaDictionary(data)  // need to form a structure to be passed in
    template.process(data, out)
    val outString = out.toString
    println(outString)
  }
}
