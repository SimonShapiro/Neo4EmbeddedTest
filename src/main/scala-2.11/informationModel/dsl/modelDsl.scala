package informationModel.dsl

import informationModel.core.{edge, edgeJson, node, nodeJson, Dsl}

/**
 * Created by simonshapiro on dd/mm/yyyy.
 */
object modelDsl extends Dsl {
  def buildNode(n: nodeJson): node = {
    n._type match {
      case "dataset" => {
        val s = dataset(n.id)
        n.properties.foreach(p => {
          p.name match {
            case "name" => s.name_(p.value)
            case "description" => s.description_(p.value)
            case "size" => s.size_(p.value.toInt)
            case _ => throw new IllegalArgumentException("%s:%s of type %s does not conform to dsl".format(p.name,p._type,p.value))
          }
        })
      s
      }
      case "system" => {
        val s = system(n.id)
        n.properties.foreach(p => {
          p.name match {
            case "name" => s.name_(p.value)
            case "description" => s.description_(p.value)
            case _ => throw new IllegalArgumentException("%s:%s of type %s does not conform to dsl".format(p.name,p._type,p.value))
          }
        })
      s
      }
      case _ => throw new IllegalArgumentException("%s:%s not in dsl".format(n.id, n._type))
    }
  }

  def buildEdge(e: edgeJson, nodes: List[node]): edge = {
    val fromNode = nodes.filter(n => (n.id == e.from)).head
    val toNode = nodes.filter(n => (n.id == e.to)).head
    e._type match {
      case "systemPRODUCESdataset" => {
        val newEdge = new systemPRODUCESdataset(fromNode.asInstanceOf[system],toNode.asInstanceOf[dataset],e.id)
        e.properties.foreach(p => {
          p.name match {
                case "frequency" => newEdge.frequency_(p.value.toInt)
            case _ => throw new IllegalArgumentException("%s:%s of type %s does not conform to dsl".format(p.name,p._type,p.value))
          }
        })
        newEdge
      }
      case "systemCONNECTSsystem" => {
        val newEdge = new systemCONNECTSsystem(fromNode.asInstanceOf[system],toNode.asInstanceOf[system],e.id)
        e.properties.foreach(p => {
          p.name match {
                case "associatedWithdataset" => newEdge.associatedWithdataset_(nodes.filter(n => n.id == p.value).head.asInstanceOf[dataset])  // is now an id on a node somewhere!!
            case _ => throw new IllegalArgumentException("%s:%s of type %s does not conform to dsl".format(p.name,p._type,p.value))
          }
        })
        newEdge
      }
      case _ => throw new IllegalArgumentException("%s:%s not in dsl".format(e.id, e._type))
    }
  }
}
