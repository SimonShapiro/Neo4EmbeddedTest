package informationModel.kernel

/**
 * Created by simonshapiro on 12/12/15.
 */

import informationModel.core.node
import scala.collection.mutable.ArrayBuffer

/**
 * Created by simonshapiro on 23/11/15.
 */

case class Property(val uid: String = null) extends node {

  val id = if (uid != null) uid else uuid

  val _type: String = "Property"

  private var _name: Option[String] = None
  def name = _name
  def name_(name:String) = {_name = Option(name) ; this}

  private var _valueType: Option[String] = None
  def valueType = _valueType
  def valueType_(valueType:String) = {_valueType = Option(valueType) ; this}

  def toJString: String = {
    val str = new ArrayBuffer[String]
    str += """ "id": "%s"""".format(id)
    str += """ "$type": "%s"""".format(_type)   // followed by an array of generalised properties (_name, _type, _valueString)
    _name match {
      case Some(st) => str += """ "name": "%s"""".format(st)
      case None =>
    }
    _valueType match {
      case Some(i) => str += """ "valueType": %s""".format(i)
      case None =>
    }
    "{" + str.mkString(",") + "}"
  }

  def deepCopy: Property = {
    val s = Property(id)
    _name match {
      case Some(st) => s.name_(st)
      case None =>
    }
    _valueType match {
      case Some(st) => s.valueType_(st)
      case None =>
    }
    s
  }

  override def isComplete: Boolean = true  // all properties optional

  override def isEqual(n: node) = {
    val d = n.asInstanceOf[Property]
    (id == d.id) && (name == d.name) && (valueType == d.valueType)
  }

  def toDyNetMLAsJString: String = {
    val str = new ArrayBuffer[String]
    val propStr = new ArrayBuffer[String]
    str += """ "id": "%s"""".format(id)
    str += """ "$type": "%s"""".format(_type)
    _name match {
      case Some(st) => propStr += propString[String]("name",st)
      case None =>
    }
    _valueType match {
      case Some(st) => propStr += propString[String]("valueType", st)
      case None =>
    }
    str += """ "properties": [""" + propStr.mkString(",") + "]"
    "{" + str.mkString(",") + "}"
  }
}
