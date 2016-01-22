package informationModel.dsl

import informationModel.core.{node, edge}
import scala.collection.mutable.ArrayBuffer

/**
 * Created by simonshapiro on 23/11/15.
 */
class systemPRODUCESREPORTreport(from: system, to: report, uid: String = null) extends edge(from, to) {

  val id = if (uid != null) uid else uuid
  memberProperties("id") = ("String", id)

  val _type: String = "system_PRODUCESREPORT_report"
  memberProperties("_type") = ("String", _type)

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

  def deepCopy: systemPRODUCESREPORTreport = {   //  strangely not copying the association - could be a problem
    val e = new systemPRODUCESREPORTreport(from, to, id)

    _frequency match {
      case Some(f) => e.frequency_(f)
      case None =>
    }
    e
  }

  override def isEqual(ee: edge) = {
    val e = ee.asInstanceOf[systemPRODUCESREPORTreport]
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
