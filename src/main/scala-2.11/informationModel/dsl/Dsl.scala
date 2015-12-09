package informationModel.dsl

import informationModel.core.{edge, edgeJson, node, nodeJson}

/**
 * Created by simonshapiro on 08/12/15.
 */
object Dsl {
  def buildNode(n: nodeJson): node = {
    n.$type match {
      case "System" => {
        val s = system(n.id)
        n.properties.foreach(p => {
          p.name match {
            case "name" => s.name_(p.value)
            case "description" => s.description_(p.value)
            case _ => throw new IllegalArgumentException("%s:%s of type %s does not conform to dsl".format(p.name,p.$type,p.value))
          }
        })
      s
      }
      case "Dataset" => {
        val d = dataset(n.id)
        n.properties.foreach(p => {
          p.name match {
            case "name" => d.name_(p.value)
            case "description" => d.description_(p.value)
            case _ => throw new IllegalArgumentException("%s:%s of type %s does not conform to dsl".format(p.name,p.$type,p.value))
          }
        })
        d
      }
      case _ => throw new IllegalArgumentException("%s:%s not in dsl".format(n.id, n.$type))
    }
  }
  def buildEdge(e: edgeJson, nodes: List[node]): edge = {
    val fromNode = nodes.filter(n => (n.id == e.from)).head
    val toNode = nodes.filter(n => (n.id == e.to)).head
    e.$type match {
      case "SystemConnectsSystem" => {
        val newEdge = new systemCONNECTSsystem(fromNode.asInstanceOf[system],toNode.asInstanceOf[system],e.id)
        e.properties.foreach(p => {
          p.name match {
            case _ => throw new IllegalArgumentException("%s:%s of type %s does not conform to dsl".format(p.name,p.$type,p.value))
          }
        })
        newEdge
      }
      case "SystemProducesDataset" => {
        val newEdge = new systemPRODUCESdataset(fromNode.asInstanceOf[system],toNode.asInstanceOf[dataset],e.id)
        e.properties.foreach(p => {
          p.name match {
            case "frequency" => newEdge.frequency_(p.value.toInt)
            case _ => throw new IllegalArgumentException("%s:%s of type %s does not conform to dsl".format(p.name,p.$type,p.value))
          }
        })
        newEdge
      }
      case _ => throw new IllegalArgumentException("%s:%s not in dsl".format(e.id, e.$type))
    }
  }
}
