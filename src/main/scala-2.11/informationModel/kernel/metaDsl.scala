package informationModel.kernel

import informationModel.core._

/**
 * Created by simonshapiro on 08/12/15.
 */
object metaDsl extends Dsl {
  def buildNode(n: nodeJson): node = {
    n._type match {
      case "MetaNode" => {
        val s = MetaNode(n.id)
        n.properties.foreach(p => {
          p.name match {
            case "name" => s.name_(p.value)
            case "description" => s.description_(p.value)
            case _ => throw new IllegalArgumentException("%s:%s of type %s does not conform to dsl".format(p.name,p._type,p.value))
          }
        })
      s
      }
      case "Property" => {
        val d = Property(n.id)
        n.properties.foreach(p => {
          p.name match {
            case "name" => d.name_(p.value)
            case "valueType" => d.valueType_(p.value)
            case _ => throw new IllegalArgumentException("%s:%s of type %s does not conform to dsl".format(p.name,p._type,p.value))
          }
        })
        d
      }
      case "MetaEdgeNode" => {
        val d = MetaEdgeNode(n.id)
        n.properties.foreach(p => {
          p.name match {
            case "name" => d.name_(p.value)
            case "description" => d.description_(p.value)
            case _ => throw new IllegalArgumentException("%s:%s of type %s does not conform to dsl".format(p.name,p._type,p.value))
          }
        })
        d
      }
      case _ => throw new IllegalArgumentException("%s:%s not in dsl".format(n.id, n._type))
    }
  }
  def buildEdge(e: edgeJson, nodes: List[node]): edge = {
    val fromNode = nodes.filter(n => (n.id == e.from)).head
    val toNode = nodes.filter(n => (n.id == e.to)).head
    e._type match {
      case "MetaNode_HASPROPERTIES_Property" => {
        val newEdge = new MetaNodeHASPROPERTIESProperty(fromNode.asInstanceOf[MetaNode],toNode.asInstanceOf[Property],e.id)
        newEdge
      }
      case "MetaEdge_HASPROPERTIES_Property" => {
        val newEdge = new MetaEdgeHASPROPERTIESProperty(fromNode.asInstanceOf[MetaEdgeNode],toNode.asInstanceOf[Property],e.id)
        newEdge
      }
      case "MetaNode_CONNECTS_MetaNode" => {
        val newEdge = new MetaNodeCONNECTSMetaNode(fromNode.asInstanceOf[MetaNode],toNode.asInstanceOf[MetaNode],e.id)
        if (e.associationNode != "") newEdge.associatedWith_(nodes.filter(n=> (n.id == e.associationNode)).head)
        else newEdge
      }
      case _ => throw new IllegalArgumentException("%s:%s not in dsl".format(e.id, e._type))
    }
  }
}
