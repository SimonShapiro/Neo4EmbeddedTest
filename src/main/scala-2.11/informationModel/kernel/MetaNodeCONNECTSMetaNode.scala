package informationModel.kernel

import informationModel.core.{node, edge}

import scala.collection.mutable.ArrayBuffer

/**
 * Created by simonshapiro on 23/11/15.
 */
class MetaNodeCONNECTSMetaNode(from: MetaNode, to: MetaNode, uid: String = null) extends edge(from, to) {

  val id = if (uid != null) uid else uuid
  
  val _type: String = "MetaNode_CONNECTS_MetaNode"

  def toJString: String = {
    val str = header
    "{" + str.mkString(",") + "}"
  }

  def deepCopy: MetaNodeCONNECTSMetaNode = {
    val e = new MetaNodeCONNECTSMetaNode(from, to, id)
    e
  }

  override def isEqual(ee: edge) = {
    val e = ee.asInstanceOf[MetaNodeCONNECTSMetaNode]
    (id == e.id) && (from == e.from) && (to == e.to)
  }

  override def isComplete: Boolean = true

  def toDyNetMLAsJString: String = {
//  val str
    val str = header
    val propStr = new ArrayBuffer[String]
    str += """ "properties": [""" + propStr.mkString(",") + "]"
    "{" + str.mkString(",") + "}"
  }
}
