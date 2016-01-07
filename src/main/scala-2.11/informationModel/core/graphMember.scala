package informationModel.core

/**
 * Created by simonshapiro on 25/11/15.
 */

import java.util.NoSuchElementException
import jdk.nashorn.api.scripting.JSObject
import play.api.libs.json.{JsObject, Json}

import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}

abstract class graphMember {

  val id: String

  override def toString = id

  def uuid = java.util.UUID.randomUUID.toString

  val _type: String

  def getType = _type

  def isOfType(t: String) = (getType == t)

  def isComplete: Boolean  // implement test for mandatory properties NOT None

  def toJString: String

  def toDyNetMLAsJString: String

  def isJsonEqual(gm: graphMember): Boolean = {
    val thisStr = this.toJString
    val gmStr = gm.toJString
    gm.toJString.equals(this.toJString)
  }

  def propString[T](nm: String, v:T): String = """ {"name": "%s", "type": "%s", "value": "%s"}""".format(nm, v.getClass.toString.split('.').last, v)

}
