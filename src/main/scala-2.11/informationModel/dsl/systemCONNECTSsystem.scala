package informationModel.dsl

import informationModel.core.{node, edge}
import scala.collection.mutable.ArrayBuffer

/**
 * Created by simonshapiro on 23/11/15.
 */

class systemCONNECTSsystem(from: system, to:system, uid: String = null) extends edge(from, to) {

  val id = if (uid != null) uid else uuid

  def isComplete: Boolean = true

  val _type: String = "systemCONNECTSsystem"

  override def associatedWith_(associationNode: node) = this  // to prevent the misuse of associatedWith_

  private var _asoociatedWithDataset: Option[dataset] = None
  def associatedWithDataset = _asoociatedWithDataset
  def associatedWithDataset_(associationNode: dataset) = {
                                          _asoociatedWithDataset = Option(associationNode)
                                          this
                                        }
  private def associatedWithDatasetEquals(a: Option[dataset], b: Option[dataset]): Boolean = {
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

  def toJString: String = {  // associated with data set - in plain view here and as property below.
    val str = header
    associatedWithDataset match {
      case Some(x) => str += """ "associatedWithDataset": "%s" """.format(x.id)
      case None =>
    }
    /*
    str += """ "associatedWithDataset": "%s" """.format(associatedWithDataset match {
                                                          case Some(x) => x.id
                                                          case None => ""
    })
    */
    "{" + str.mkString(",") + "}"
  }

  def deepCopy: systemCONNECTSsystem = {
    val e = new systemCONNECTSsystem(from, to, id)
    e
  }

  override def isEqual(ee: edge) = {
    val e = ee.asInstanceOf[systemCONNECTSsystem]
    (id == e.id) &&
      (from == e.from) &&
      (to == e.to)  &&
      associatedWithDatasetEquals(associatedWithDataset,e.associatedWithDataset)
      // && (description == e.description)
  }

  def toDyNetMLAsJString: String = {
    val str = header
    val propStr = new ArrayBuffer[String]
    associatedWithDataset match {
      case Some(x) => propStr += propString[dataset]("associatedWithDataset",x)
      case None =>
    }
    str += """ "properties": [""" + propStr.mkString(",") + "]"
    "{" + str.mkString(",") + "}"
  }
}

