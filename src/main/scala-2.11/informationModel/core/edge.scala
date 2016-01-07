package informationModel.core

import scala.collection.mutable.ArrayBuffer

/**
 * Created by simonshapiro on 23/11/15.
 */

abstract class edge (val from: node, val to: node) extends graphMember{  // consider ', val _with: node' as an extention of the signature to provide in-line properties

  private var _asoociatedWith: Option[node] = None
  def associatedWith = _asoociatedWith
  def associatedWith_(associationNode: node) = {_asoociatedWith = Option(associationNode); this}  // generate an override to prevent use outside the meta context

  def deepCopy: edge
  def isEqual(e: edge): Boolean

  def header: ArrayBuffer[String] = {
    val str = new ArrayBuffer[String]
    str += """ "id": "%s"""".format(id)
    str += """ "$type": "%s"""".format(_type)
    str += """ "from":  "%s"""".format(from.id)
    str += """ "to":  "%s"""".format(to.id)
    associatedWith match {
      case Some(x) => str +=  """ "associationNode":  "%s"""".format(x.id)
      case None => str += """ "associationNode":  "" """
    }
    str
  }

}
