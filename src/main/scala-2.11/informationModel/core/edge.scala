package informationModel.core

import informationModel.core.node

/**
 * Created by simonshapiro on 23/11/15.
 */
abstract class edge (val from: node,val to: node) extends graphMember{
  def deepCopy: edge
  def hasSameId(e: edge): Boolean = e.id == id
}
