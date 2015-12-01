package informationModel

abstract class node extends graphMember {
  def deepCopy: node
  def hasSameId(n: node): Boolean = n.id == id

}

