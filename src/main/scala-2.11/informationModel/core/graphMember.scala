package informationModel.core

/**
 * Created by simonshapiro on 25/11/15.
 */

import java.util.NoSuchElementException

import informationModel.core.propertyChatacteristics.propertyChatacteristics
import org.json4s.native.Serialization.{write => swrite}

import scala.collection.{immutable, mutable}

trait graphMember {

  val withProperties: propertyChatacteristics

  private val properties = new mutable.HashMap[String, Any]

  val id: String

  def uuid = java.util.UUID.randomUUID.toString

  def hasType = getClass.toString.split('.').last

  def isOfType(t: String) = (hasType == t)

  val manifest: immutable.HashMap[String, (String, String)]

  def isComplete: Boolean

//  def setProperty(k: String, v: Any): this.type

  def getProperty(k: String) = properties(k)

  def getAllProperties = properties


  def hasEqualProperties(p: mutable.HashMap[String, Any]): Boolean = {
    if (properties.size != p.size) false
    else try {
      properties.map(prop => p(prop._1) == prop._2).foldLeft(true)((r, c) => r && c) // this will give trouble if off manifest property table have the same size but different optionals from an open manifest
    }
    catch {
      case e: NoSuchElementException => false
    }
  }

  def setProperty(manifestType: propertyChatacteristics, k: String, v: Any) = {
    if (manifest.contains(k)) {
      val keyType = manifest(k)._1
      val valueType = v.getClass.getCanonicalName
      if (keyType == valueType) properties(k) = v
      else throw new IllegalArgumentException(""" Expecting type "%s" received type "%s" for "%s" """.format(keyType,valueType,k))
    } // test for type as well
    else manifestType match {
      case propertyChatacteristics.open   => properties(k) = v
      case propertyChatacteristics.closed => throw new IllegalArgumentException(""" "%s" is not on manifest""".format(k))
      case propertyChatacteristics.none   => throw new IllegalArgumentException(""" "%s" property is not allowed on members with 'none' set for manifesto """.format(k))
    }
    this
  }

  def isIdenticalTo(manifestType: propertyChatacteristics, gm: graphMember) = {
    var identical = false
    val equalIds = gm.id == id
    manifestType match {
      case propertyChatacteristics.open   => equalIds && hasEqualProperties(gm.getAllProperties)
      case propertyChatacteristics.closed => equalIds && hasEqualProperties(gm.getAllProperties)
      case propertyChatacteristics.none   => equalIds
    }
  }

}
