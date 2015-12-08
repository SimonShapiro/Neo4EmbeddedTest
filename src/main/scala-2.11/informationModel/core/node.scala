package informationModel.core

abstract class node extends graphMember {
  def deepCopy: node
  def isEqual(n: node): Boolean
}

