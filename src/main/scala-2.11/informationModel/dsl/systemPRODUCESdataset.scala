package informationModel.dsl

import informationModel.core.edge
import scala.collection.mutable.ArrayBuffer

/**
 * Created by simonshapiro on 23/11/15.
 */
class systemPRODUCESdataset(from: system, to: dataset, uid: String = null) extends edge(from, to) {

  val id = if (uid != null) uid else uuid
  
  val _type: String = "SystemProducesDataset"

  private var _frequency: Option[Int] = None
  def frequency = _frequency
  def frequency_(frequency: Int) = {_frequency = Option(frequency) ; this}

  def toJString: String = {
    val str = header
    "{" + str.mkString(",") + "}"
  }

  def deepCopy: systemPRODUCESdataset = {
    val e = new systemPRODUCESdataset(from, to, id)
    _frequency match {
      case Some(f) => e.frequency_(f)
      case None =>
    }
    e
  }

  override def isEqual(ee: edge) = {
    val e = ee.asInstanceOf[systemPRODUCESdataset]
    (id == e.id) && (from == e.from) && (to == e.to)  && (frequency == e.frequency)
  }

  override def isComplete: Boolean = true

  def toDyNetMLAsJString: String = {
    val str = header
    val propStr = new ArrayBuffer[String]
    _frequency match {
      case Some(i) => propStr += propString[Int]("frequency",i)
      case None =>
    }
    str += """ "properties": [""" + propStr.mkString(",") + "]"
    "{" + str.mkString(",") + "}"
  }
}
