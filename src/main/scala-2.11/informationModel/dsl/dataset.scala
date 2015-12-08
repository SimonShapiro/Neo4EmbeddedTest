package informationModel.dsl

import informationModel.core.{node}

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by simonshapiro on 23/11/15.
 */

case class dataset(val uid: String = null) extends node {
  val id = if (uid != null) uid else uuid
  val $type: String = "Dataset"
  def isComplete = true

  private var _name: Option[String] = None
  def name = _name
  def name_(name:String) = {_name = Option(name) ; this}

  private var _description: Option[String] = None
  def description = _description
  def description_(description:String) = {_description = Option(description) ; this}

  def toJString: String = {
    val str = new ArrayBuffer[String]
    str += """ "id": "%s"""".format(id)
    str += """ "$type": "%s"""".format($type)
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

  def deepCopy: dataset = {
    val s = dataset(id)
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

  override def isEqual(dd: node) = {
    val d = dd.asInstanceOf[dataset]
    (id == d.id) && (name == d.name) && (description == d.description)
  }
}
