package informationModel

import scala.collection.immutable

/**
 * Created by simonshapiro on 23/11/15.
 */

case class system(val uid: String = null) extends node {

  val id = if (uid != null) uid else uuid

  val withProperties = propertyChatacteristics.open

  override val manifest = immutable.HashMap(
    "name" -> ("java.lang.String","req"),
    "description" -> ("java.lang.String","opt")
  )

  override def isComplete: Boolean = true

  def CONNECTS(s: system, id: String = null) = {
    new systemCONNECTSsystem(this, s, id)
  }

  def PRODUCES(d: dataset, id: String = null) = {
    new systemPRODUCESdataset(this, d, id)
  }

  def <= (kv: (String, Any)): system = {
    setProperty(withProperties, kv._1, kv._2)
    this
  }

  def deepCopy = {
    val s = new system(id)
    getAllProperties.foreach(p => s.<=(p._1, p._2))
    s
  }

  def isIdenticalTo(s: system) = {
    hasSameId(s) && hasEqualProperties(s.getAllProperties)
  }
}
