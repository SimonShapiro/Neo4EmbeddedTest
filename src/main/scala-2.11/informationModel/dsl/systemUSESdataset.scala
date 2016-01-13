package informationModel.dsl

import informationModel.core.{node, edge}
import scala.collection.mutable.ArrayBuffer

/**
 * Created by simonshapiro on 23/11/15.
 */
class systemUSESdataset(from: system, to: dataset, uid: String = null) extends edge(from, to) {

  val id = if (uid != null) uid else uuid

  val _type: String = "systemUSESdataset"


  def toJString: String = {
    val str = header
    "{" + str.mkString(",") + "}"
  }

  def deepCopy: systemUSESdataset = {   //  strangely not copying the association - could be a problem
    val e = new systemUSESdataset(from, to, id)

    e
  }

  override def isEqual(ee: edge) = {
    val e = ee.asInstanceOf[systemUSESdataset]
    ((id == e.id)
    && (from == e.from)
    && (to == e.to)
    )
  }

  override def isComplete: Boolean = true

  def toDyNetMLAsJString: String = {
    val str = header
    val propStr = new ArrayBuffer[String]
    str += """ "properties": [""" + propStr.mkString(",") + "]"
    "{" + str.mkString(",") + "}"
  }
}
