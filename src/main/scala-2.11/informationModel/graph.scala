package informationModel

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

  def isSubGraphOf(g: graph) = {  // if all members are also members of g
    val nodeTest = nodes.map(n => {
      if (g.nodes.contains(n._1)) n._2.isIdenticalTo(n._2.withProperties, g.nodes(n._1))
      else false
    })
    val nodesIn = nodeTest.foldLeft(true)((r, c) => r && c)
    val edgeTest = edges.map(e => {
      if (g.edges.contains(e._1)) e._2.isIdenticalTo(e._2.withProperties, g.edges(e._1))
    })
    val edgesIn = edgeTest.foldLeft(true)((r, c) => r && c.asInstanceOf[Boolean])
    nodesIn && edgesIn
  }
}
