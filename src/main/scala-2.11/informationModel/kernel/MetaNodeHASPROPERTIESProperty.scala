package informationModel.kernel

import informationModel.core.edge

import scala.collection.mutable.ArrayBuffer

/**
 * Created by simonshapiro on 23/11/15.
 */
class MetaNodeHASPROPERTIESProperty(from: MetaNode, to: Property, uid: String = null) extends edge(from, to) {

  val id = if (uid != null) uid else uuid
  
  val _type: String = "MetaNodeHASPROPERTIESProperty"

  def toJString: String = {
    val str = header
    "{" + str.mkString(",") + "}"
  }

  def deepCopy: MetaNodeHASPROPERTIESProperty = {
    val e = new MetaNodeHASPROPERTIESProperty(from, to, id)
    e
  }

  override def isEqual(ee: edge) = {
    val e = ee.asInstanceOf[MetaNodeHASPROPERTIESProperty]
    (id == e.id) && (from == e.from) && (to == e.to)
  }

  override def isComplete: Boolean = true

  def toDyNetMLAsJString: String = {
    val str = header
    val propStr = new ArrayBuffer[String]
    str += """ "properties": [""" + propStr.mkString(",") + "]"
    "{" + str.mkString(",") + "}"
  }
}
