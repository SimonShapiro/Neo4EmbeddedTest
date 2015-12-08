package informationModel.dsl

import informationModel.core.node
import scala.collection.mutable.ArrayBuffer

/**
 * Created by simonshapiro on 23/11/15.
 */

case class system(val uid: String = null) extends node {

  val id = if (uid != null) uid else uuid

  val $type: String = "System"

  private var _name: Option[String] = None
  def name = _name
  def name_(name:String) = {_name = Option(name) ; this}

  private var _description: Option[String] = None
  def description = _description
  def description_(description:String) = {_description = Option(description) ; this}

  def CONNECTS(s: system, id: String = null) = {
    new systemCONNECTSsystem(this, s, id)
  }

  def PRODUCES(d: dataset, id: String = null) = {
    new systemPRODUCESdataset(this, d, id)
  }

  def toJString: String = {
    val str = new ArrayBuffer[String]
    str += """ "id": "%s"""".format(id)
    str += """ "$type": "%s"""".format($type)   // followed by an array of generalised properties (_name, _type, _valueString)
    _name match {
      case Some(st) => str += """ "name": "%s"""".format(st)
      case None =>
    }
    _description match {
      case Some(i) => str += """ "description": %s""".format(i)
      case None =>
    }
    "{" + str.mkString(",") + "}"
  }

  def deepCopy: system = {
    val s = system(id)
    _name match {
      case Some(st) => s.name_(st)
      case None =>
    }
    _description match {
      case Some(st) => s.description_(st)
      case None =>
    }
    s
  }

  override def isComplete: Boolean = true  // all properties optional

  override def isEqual(n: node) = {
    val d = n.asInstanceOf[system]
    (id == d.id) && (name == d.name) && (description == d.description)
  }

  def toDyNetMLAsJString: String = {
    val str = new ArrayBuffer[String]
    val propStr = new ArrayBuffer[String]
    str += """ "id": "%s"""".format(id)
    str += """ "$type": "%s"""".format($type)
    _name match {
      case Some(st) => propStr += propString[String]("name",st)
      case None =>
    }
    _description match {
      case Some(st) => propStr += propString[String]("description", st)
      case None =>
    }
    str += """ "properties": [""" + propStr.mkString(",") + "]"
    "{" + str.mkString(",") + "}"
  }
}
