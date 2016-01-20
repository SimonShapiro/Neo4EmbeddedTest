package informationModel.dsl

import informationModel.core.{node, edge}
import scala.collection.mutable.ArrayBuffer

/**
 * Created by simonshapiro on 23/11/15.
 */
class businessUnitPROVIDESdataset(from: businessUnit, to: dataset, uid: String = null) extends edge(from, to) {

  val id = if (uid != null) uid else uuid

  val _type: String = "businessUnit_PROVIDES_dataset"

      private var _frequency: Option[Integer] = None
      def frequency = _frequency
      def frequency_(frequency: Integer) = {
        _frequency = Option(frequency)
        memberProperties("frequency") = ("Integer",frequency.toString())
        this
      }

  def toJString: String = {
    val str = header
        _frequency match {
          case Some(st) => str += """ "frequency": %s""".format(st)  //may need some shaping here around Integer
          case None =>
        }
    "{" + str.mkString(",") + "}"
  }

  def deepCopy: businessUnitPROVIDESdataset = {   //  strangely not copying the association - could be a problem
    val e = new businessUnitPROVIDESdataset(from, to, id)

    _frequency match {
      case Some(f) => e.frequency_(f)
      case None =>
    }
    e
  }

  override def isEqual(ee: edge) = {
    val e = ee.asInstanceOf[businessUnitPROVIDESdataset]
    ((id == e.id)
    && (from == e.from)
    && (to == e.to)
      && (frequency == e.frequency)
    )
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
