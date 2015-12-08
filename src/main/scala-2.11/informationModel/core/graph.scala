package informationModel.core

import informationModel.dsl.system
import play.api.libs.json._

import scala.collection.mutable

/**
 * Created by simonshapiro on 23/11/15.
 */

class graph {
  val nodes = new mutable.HashMap[String, node]
  val edges = new mutable.HashMap[String, edge]

  def <= (n: node) = {  // need to be comfortable that we simply replace new for old
//    if (!nodes.contains(n)) nodes += n
    nodes(n.id) = n
  }

  def <=> (e: edge) = {  // need to ensure both ends are in nodes
    val from = e.from
    val to = e.to
    if (!nodes.contains(from.id)) nodes(from.id) = from
    if (!nodes.contains(to.id)) nodes(to.id) = to
//    if (!edges.contains(e)) edges += e
    edges(e.id) = e
  }

  def deepCopy = {
    val gCopy = new graph
    nodes.foreach(n => {
      gCopy.nodes(n._1) = n._2.deepCopy
    })
    edges.foreach(e => gCopy.edges += e)
    gCopy
  }

  def getNode(k: String) = {
    nodes(k)
  }
  def getEdge(k: String) = {
    edges(k)
  }

  def filterOnNodeType(t: String) = {
    val sg = new graph
    nodes.foreach(n => {
      if (n._2.isOfType(t)) {
        val newNode = n._2.deepCopy
        sg <= newNode
      }
    })
    edges.foreach(e => {
      if (e._2.from.isOfType(t)) sg <=> e._2.deepCopy
      if (e._2.to.isOfType(t)) sg <=> e._2.deepCopy
    })
    sg
  }

  def isSubGraphOf(g: graph): Boolean = {  // if all members are also members of g
    val nodeTest = nodes.map(n => {
      if (g.nodes.contains(n._1)) n._2.isJsonEqual(g.nodes(n._1))   // testing for equality might not be deep enough
      else false
    })
    val nodesIn = nodeTest.foldLeft(true)((r, c) => r && c)
    val edgeTest = edges.map(e => {
      if (g.edges.contains(e._1)) e._2.isJsonEqual(g.edges(e._1))
      else false
    })
    val edgesIn = edgeTest.foldLeft(true)((r, c) => r && c)
    nodesIn && edgesIn
  }

  def isEqualTo(g: graph) = {  // if a sub-graph and node size and edge size matches
    isSubGraphOf(g) && (nodes.size == g.nodes.size) && (edges.size == g.edges.size)
  }

  def toJson = {
    val nodesMap = nodes.map(n => Json.parse(n._2.toJString))
    val edgesMap = edges.map(e => Json.parse(e._2.toJString))
    val jsonInternal =
      Json.obj ("graph" -> Json.obj(
                  "nodes" -> nodesMap,
                  "edges" -> edgesMap)
      )
    val nodesBasedOnJson = jsonInternal \ "graph" \ "nodes"
//    val sz = nodesBasedOnJson
//    nodesBasedOnJson.foreach(p => {
//      val props = p.children.children
//      println(p.values.getClass, p.getClass)
//    })
    val cls = nodesBasedOnJson.getClass
    val edgesBasedonJson = jsonInternal \ "graph" \ "edges"
//    val rjson = compact(render(json))
    val n = nodesBasedOnJson.get(0) \ "id"
    val i = n.as[String]
    val p = (nodesBasedOnJson.get(1) \ "properties")
    println("pause")
//    val tGraph = parse(rjson)  // defensive parsing to ensure json created correctly
    jsonInternal.toString
  }

  def this(json: String) {
    this
    val jsonInternal = Json.parse(json)
//    val nodeList = (JsPath \ "graph" \ "nodes" \ "id")(jsonInternal)(0)
    val customReader: Reads[List[String]] = (__ \ "graph" \ "nodes").read[List[String]]
    val r = customReader.reads(jsonInternal)
    r.fold(
      valid = { res =>
        val s: List[String] = res
        s.foreach(i => println(i))
      },
      invalid = { errors => println(errors)}
    )
    println("end...")
    /*
    val edgelist = (jsonInternal \ "graph" \ "edges").get
    //  get an effective loop through nodes here maybe a map and pass the structure to a node factory
//    val n = nodeList.as[Array[Node]]
    implicit val nodesReader: Reads[(String)] =
        (JsPath \ "id").read[String]
    val ns = nodeList.as[List[String]]
    val nId = (nodeList.head.get \ "id").get.as[String]
*/
    println("pause")
  }
}
