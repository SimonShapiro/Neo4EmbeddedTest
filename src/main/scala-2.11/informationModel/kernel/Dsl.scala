package informationModel.kernel

import informationModel.core.{edge, edgeJson, node, nodeJson}

/**
 * Created by simonshapiro on 08/12/15.
 */
object Dsl {
  def buildNode(n: nodeJson): node = {
    n._type match {
      case "MetaModel" => {
        val s = MetaNode(n.id)
        n.properties.foreach(p => {
          p.name match {
            case "name" => s.name_(p.value)
            case "description" => s.description_(p.value)
            case _ => throw new IllegalArgumentException("%s:%s of type %s does not conform to dsl".format(p.name,p.$type,p.value))
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
            case _ => throw new IllegalArgumentException("%s:%s of type %s does not conform to dsl".format(p.name,p.$type,p.value))
          }
        })
        d
      }
      case "MetaEdge" => {
        val d = MetaEdge(n.id)
        n.properties.foreach(p => {
          p.name match {
            case "name" => d.name_(p.value)
            case "description" => d.description_(p.value)
            case _ => throw new IllegalArgumentException("%s:%s of type %s does not conform to dsl".format(p.name,p.$type,p.value))
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
    e.$type match {
      case "MetaNodeHASPROPERTIESProperty" => {
        val newEdge = new MetaNodeHASPROPERTIESProperty(fromNode.asInstanceOf[MetaNode],toNode.asInstanceOf[Property],e.id)
        newEdge
      }
      case "MetaNodeOUTBOUNDMetaEdge" => {
        val newEdge = new MetaNodeOUTBOUNDMetaEdge(fromNode.asInstanceOf[MetaNode],toNode.asInstanceOf[MetaEdge],e.id)
        newEdge
      }
      case "MetaNodeINBOUNDMetaEdge" => {
        val newEdge = new MetaNodeINBOUNDMetaEdge(fromNode.asInstanceOf[MetaNode],toNode.asInstanceOf[MetaEdge],e.id)
        newEdge
      }
      case _ => throw new IllegalArgumentException("%s:%s not in dsl".format(e.id, e.$type))
    }
  }
}
