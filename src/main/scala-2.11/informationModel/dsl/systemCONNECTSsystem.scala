package informationModel.dsl

import informationModel.core.edge
import scala.collection.mutable.ArrayBuffer

/**
 * Created by simonshapiro on 23/11/15.
 */

class systemCONNECTSsystem(from: system, to:system, uid: String = null) extends edge(from, to) {

  val id = if (uid != null) uid else uuid

  def isComplete: Boolean = true

  val _type: String = "SystemConnectsSystem"

  private var _description: Option[String] = None
  def description = _description
  def description_(description:String) = {_description = Option(description) ; this}

  def toJString: String = {
    val str = new ArrayBuffer[String]
    str += """ "id": "%s"""".format(id)
    str += """ "$type": "%s"""".format(_type)
    str += """ "from":  "%s"""".format(from.id)
    str += """ "to":  "%s"""".format(to.id)
    _description match {
      case Some(i) => str += """ "description": %s""".format(i)
      case None =>
    }
    "{" + str.mkString(",") + "}"
  }

  def deepCopy: systemCONNECTSsystem = {
    val e = new systemCONNECTSsystem(from, to, id)
    e
  }

  override def isEqual(ee: edge) = {
    val e = ee.asInstanceOf[systemCONNECTSsystem]
    (id == e.id) && (from == e.from) && (to == e.to)  && (description == e.description)
  }

  def toDyNetMLAsJString: String = {
    val str = new ArrayBuffer[String]
    val propStr = new ArrayBuffer[String]
    str += """ "id": "%s"""".format(id)
    str += """ "$type": "%s"""".format(_type)
    str += """ "from":  "%s"""".format(from.id)
    str += """ "to":  "%s"""".format(to.id)
    _description match {
      case Some(st) => propStr += propString[String]("description", st)
      case None =>
    }
    str += """ "properties": [""" + propStr.mkString(",") + "]"
    "{" + str.mkString(",") + "}"
  }
}

