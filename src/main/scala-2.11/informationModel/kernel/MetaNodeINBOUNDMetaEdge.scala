package informationModel.kernel

import informationModel.core.edge

import scala.collection.mutable.ArrayBuffer

/**
 * Created by simonshapiro on 23/11/15.
 */
class MetaNodeINBOUNDMetaEdge(from: MetaNode, to: MetaEdge, uid: String = null) extends edge(from, to) {

  val id = if (uid != null) uid else uuid
  
  val _type: String = "MetaNodeINBOUNDMetaEdge"

  def toJString: String = {
    val str = new ArrayBuffer[String]
    str += """ "id": "%s"""".format(id)
    str += """ "$type": "%s"""".format(_type)
    str += """ "from":  "%s"""".format(from.id)
    str += """ "to":  "%s"""".format(to.id)
    "{" + str.mkString(",") + "}"
  }

  def deepCopy: MetaNodeINBOUNDMetaEdge = {
    val e = new MetaNodeINBOUNDMetaEdge(from, to, id)
    e
  }

  override def isEqual(ee: edge) = {
    val e = ee.asInstanceOf[MetaNodeINBOUNDMetaEdge]
    (id == e.id) && (from == e.from) && (to == e.to)
  }

  override def isComplete: Boolean = true

  def toDyNetMLAsJString: String = {
    val str = new ArrayBuffer[String]
    val propStr = new ArrayBuffer[String]
    str += """ "id": "%s"""".format(id)
    str += """ "$type": "%s"""".format(_type)
    str += """ "from":  "%s"""".format(from.id)
    str += """ "to":  "%s"""".format(to.id)
    str += """ "properties": [""" + propStr.mkString(",") + "]"
    "{" + str.mkString(",") + "}"
  }
}
