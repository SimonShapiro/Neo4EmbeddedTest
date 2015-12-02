package informationModel.dsl

import informationModel.core.{edge, propertyChatacteristics}
import informationModel.dsl.systemPRODUCESdataset

import scala.collection.immutable.HashMap

/**
 * Created by simonshapiro on 23/11/15.
 */

class systemCONNECTSsystem(from: system, to:system, uid: String = null) extends edge(from, to) {  // no properties to clone as it does not include a manifest
  val id = if (uid != null) uid else uuid
  override val withProperties = propertyChatacteristics.none
  override def isComplete: Boolean = true
  override val manifest = new HashMap[String, (String, String)]

  def deepCopy = {
    val e = new systemCONNECTSsystem(from.deepCopy, to.deepCopy, id)
    e
  }
  def isIdenticalTo(spd: systemPRODUCESdataset) = {
    hasSameId(spd) // && hasEqualProperties(spd.getAllProperties)
  }

  def <= (kv: (String, Any)) = {
    setProperty(withProperties, kv._1,kv._2)
    this
  }
}

