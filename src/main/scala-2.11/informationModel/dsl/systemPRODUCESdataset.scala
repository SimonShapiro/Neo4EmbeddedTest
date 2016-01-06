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
    val str = new ArrayBuffer[String]
    str += """ "id": "%s"""".format(id)
    str += """ "$type": "%s"""".format(_type)
    str += """ "from":  "%s"""".format(from.id)
    str += """ "to":  "%s"""".format(to.id)
    _frequency match {
      case Some(f) => str += """ "frequency": %s""".format(f)
      case None =>
    }
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
    val str = new ArrayBuffer[String]
    val propStr = new ArrayBuffer[String]
    str += """ "id": "%s"""".format(id)
    str += """ "$type": "%s"""".format(_type)
    str += """ "from":  "%s"""".format(from.id)
    str += """ "to":  "%s"""".format(to.id)
    _frequency match {
      case Some(i) => propStr += propString[Int]("frequency",i)
      case None =>
    }
    str += """ "properties": [""" + propStr.mkString(",") + "]"
    "{" + str.mkString(",") + "}"
  }
}
