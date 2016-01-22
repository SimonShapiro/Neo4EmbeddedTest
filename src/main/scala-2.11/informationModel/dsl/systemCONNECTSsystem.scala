package informationModel.dsl

import informationModel.core.{node, edge}
import scala.collection.mutable.ArrayBuffer

/**
 * Created by simonshapiro on 23/11/15.
 */
class systemCONNECTSsystem(from: system, to: system, uid: String = null) extends edge(from, to) {

  val id = if (uid != null) uid else uuid
  memberProperties("id") = ("String", id)

  val _type: String = "system_CONNECTS_system"
  memberProperties("_type") = ("String", _type)

        override def associatedWith_(associationNode: node) = this  // to prevent the misuse of associatedWith_
      
        private var _associatedWithdataset: Option[dataset] = None
        def associatedWithdataset = _associatedWithdataset
        def associatedWithdataset_(associationNode: dataset) = {
                                                _associatedWithdataset = Option(associationNode)
                                                memberProperties("associatedWithdataset") = ("dataset",associationNode.id)
                                                this
                                              }
        private def associatedWithdatasetEquals(a: Option[dataset], b: Option[dataset]): Boolean = {
          a match {
            case Some(x) => b match {
              case Some(y) => x.isEqual(y)
              case None    => false
            }
            case None =>  b match {
              case Some (y) => false
              case None     => true
            }
          }
        }

  def toJString: String = {
    val str = header
      associatedWithdataset match {
            case Some(x) => str += """ "associatedWithdataset": "%s" """.format(x.id)
            case None =>
      }
    "{" + str.mkString(",") + "}"
  }

  def deepCopy: systemCONNECTSsystem = {   //  strangely not copying the association - could be a problem
    val e = new systemCONNECTSsystem(from, to, id)

  // placeholder for assoc class copy
    e
  }

  override def isEqual(ee: edge) = {
    val e = ee.asInstanceOf[systemCONNECTSsystem]
    ((id == e.id)
    && (from == e.from)
    && (to == e.to)
      && associatedWithdatasetEquals(associatedWithdataset,e.associatedWithdataset)
    )
  }

  override def isComplete: Boolean = true

  def toDyNetMLAsJString: String = {
    val str = header
    val propStr = new ArrayBuffer[String]
      _associatedWithdataset match {
      case Some(i) => propStr += propString[dataset]("associatedWithdataset",i)
      case None =>
      }
    str += """ "properties": [""" + propStr.mkString(",") + "]"
    "{" + str.mkString(",") + "}"
  }
}
