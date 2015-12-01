package informationModel

import informationModel.propertyChatacteristics.propertyChatacteristics

import scala.collection.immutable
import scala.collection.immutable.HashMap

/**
 * Created by simonshapiro on 23/11/15.
 */
class systemPRODUCESdataset(from: system, to: dataset, uid: String = null) extends edge(from, to) {

  val id = if (uid != null) uid else uuid

  override val withProperties = propertyChatacteristics.closed

  override val manifest = immutable.HashMap(
                            "frequency" -> ("java.lang.Integer","opt")
                          )

  override def isComplete: Boolean = true

  def <= (kv:(String, Any)) = {
    setProperty(withProperties, kv._1,kv._2)
    this
  }

  def deepCopy = {
    val e = new systemPRODUCESdataset(from.deepCopy, to.deepCopy, id)
    getAllProperties.foreach(p => e.setProperty(withProperties, p._1,p._2))
    e
  }

  def isIdenticalTo(spd: systemPRODUCESdataset) = {
    hasSameId(spd) && hasEqualProperties(spd.getAllProperties)
  }

}
