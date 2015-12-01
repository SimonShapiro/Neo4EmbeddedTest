package informationModel

import java.util.NoSuchElementException

import scala.collection.immutable
import scala.collection.mutable

/**
 * Created by simonshapiro on 25/11/15.
 */
trait manifest {
  val properties = new mutable.HashMap[String, Any]
  val manifest: immutable.HashMap[String, (String, String)]
  def isComplete: Boolean
  def setProperty(k: String, v: Any): this.type
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
}
