package informationModel.core

/**
 * Created by simonshapiro on 23/11/15.
 */

abstract class edge (val from: node, val to: node) extends graphMember{  // consider ', val _with: node' as an extention of the signature to provide in-line properties

  private var _asoociatedWith: Option[node] = None
  def associatedWith = _asoociatedWith
  def associatedWith_(associationNode: node) = {_asoociatedWith = Option(associationNode); this}

  def deepCopy: edge
  def isEqual(e: edge): Boolean
}
