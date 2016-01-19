package informationModel.dsl

import informationModel.core.node
import scala.collection.mutable.ArrayBuffer

/**
 * Created by simonshapiro on dd/mm/yyyy.
 */

case class dataset(val uid: String = null) extends node {

  val id = if (uid != null) uid else uuid

  val _type: String = "dataset"

    private var _name: Option[String] = None
    def name = _name
    def name_(name:String) = {
        _name = Option(name)
        memberProperties("name") = ("String",name.toString())
        this
    }

    private var _description: Option[String] = None
    def description = _description
    def description_(description:String) = {
        _description = Option(description)
        memberProperties("description") = ("String",description.toString())
        this
    }

    private var _oraNodeClassType: Option[String] = None
    def oraNodeClassType = _oraNodeClassType
    def oraNodeClassType_(oraNodeClassType:String) = {
        _oraNodeClassType = Option(oraNodeClassType)
        memberProperties("oraNodeClassType") = ("String",oraNodeClassType.toString())
        this
    }

    private var _size: Option[Integer] = None
    def size = _size
    def size_(size:Integer) = {
        _size = Option(size)
        memberProperties("size") = ("Integer",size.toString())
        this
    }


  def toJString: String = {
    val str = new ArrayBuffer[String]
    str += """ "id": "%s"""".format(id)
    str += """ "$type": "%s"""".format(_type)   // followed by an array of generalised properties (_name, _type, _valueString)
        _name match {
          case Some(st) => str += """ "name": "%s"""".format(st)  //may need some shaping here around String
          case None =>
        }

        _description match {
          case Some(st) => str += """ "description": "%s"""".format(st)  //may need some shaping here around String
          case None =>
        }

        _oraNodeClassType match {
          case Some(st) => str += """ "oraNodeClassType": "%s"""".format(st)  //may need some shaping here around String
          case None =>
        }

        _size match {
          case Some(st) => str += """ "size": "%s"""".format(st)  //may need some shaping here around Integer
          case None =>
        }

    "{" + str.mkString(",") + "}"
  }

  def deepCopy: dataset = {
    val s = dataset(id)
        _name match {
          case Some(x) => s.name_(x)
          case None =>
        }
        _description match {
          case Some(x) => s.description_(x)
          case None =>
        }
        _oraNodeClassType match {
          case Some(x) => s.oraNodeClassType_(x)
          case None =>
        }
        _size match {
          case Some(x) => s.size_(x)
          case None =>
        }
    s
  }

  override def isComplete: Boolean = true  // all properties optional

  override def isEqual(n: node) = {
    val d = n.asInstanceOf[dataset]
    ((id == d.id)
        && (name== d.name)
        && (description== d.description)
        && (oraNodeClassType== d.oraNodeClassType)
        && (size== d.size)
    )
  }

  def toDyNetMLAsJString: String = {
    val str = new ArrayBuffer[String]
    val propStr = new ArrayBuffer[String]
    str += """ "id": "%s"""".format(id)
    str += """ "$type": "%s"""".format(_type)
        _name match {
          case Some(x) => propStr += propString[String]("name",x)
          case None =>
        }
        _description match {
          case Some(x) => propStr += propString[String]("description",x)
          case None =>
        }
        _oraNodeClassType match {
          case Some(x) => propStr += propString[String]("oraNodeClassType",x)
          case None =>
        }
        _size match {
          case Some(x) => propStr += propString[Integer]("size",x)
          case None =>
        }
    str += """ "properties": [""" + propStr.mkString(",") + "]"
    "{" + str.mkString(",") + "}"
  }

}
