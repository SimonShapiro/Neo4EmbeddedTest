package informationModel.dsl

import informationModel.core.{node, propertyChatacteristics}

import scala.collection.immutable

/**
 * Created by simonshapiro on 23/11/15.
 */

case class dataset(val uid: String = null) extends node {
  val id = if (uid != null) uid else uuid

  val withProperties = propertyChatacteristics.open

  override val manifest = immutable.HashMap(
                            "name" -> ("java.lang.String","req"),
                            "description" -> ("java.lang.String","opt")
                          )
  override def isComplete = true

  def <= (kv: (String, Any)): dataset = {
    setProperty(withProperties, kv._1,kv._2)
    this
  }

  def deepCopy = {
    val d = new dataset(id)
    getAllProperties.foreach(p => d.setProperty(withProperties, p._1,p._2))
    d
  }

  def isIdenticalTo(d: dataset) = {
    hasSameId(d) && hasEqualProperties(d.getAllProperties)
  }

}
