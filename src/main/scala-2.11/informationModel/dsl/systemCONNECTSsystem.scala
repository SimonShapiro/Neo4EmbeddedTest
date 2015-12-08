package informationModel.dsl

import informationModel.core.edge
import scala.collection.mutable.ArrayBuffer

/**
 * Created by simonshapiro on 23/11/15.
 */

class systemCONNECTSsystem(from: system, to:system, uid: String = null) extends edge(from, to) {

  val id = if (uid != null) uid else uuid

  def isComplete: Boolean = true

  val $type: String = "SystemConnectsSystem"

  def toJString: String = {
    val str = new ArrayBuffer[String]
    str += """ "id": "%s"""".format(id)
    str += """ "$type": "%s"""".format($type)
    str += """ "from":  "%s"""".format(from.id)
    str += """ "to":  "%s"""".format(to.id)
    "{" + str.mkString(",") + "}"
  }

  def deepCopy: systemCONNECTSsystem = {
    val e = new systemCONNECTSsystem(from, to, id)
    e
  }

  override def isEqual(ee: edge) = {
    val e = ee.asInstanceOf[systemCONNECTSsystem]
    (id == e.id) && (from == e.from) && (to == e.to)
  }

  def toDyNetMLAsJString: String = {
    val str = new ArrayBuffer[String]
    val propStr = new ArrayBuffer[String]
    str += """ "id": "%s"""".format(id)
    str += """ "$type": "%s"""".format($type)
    str += """ "from":  "%s"""".format(from.id)
    str += """ "to":  "%s"""".format(to.id)
    str += """ "properties": [""" + propStr.mkString(",") + "]"
    "{" + str.mkString(",") + "}"
  }
}

