package informationModel.dsl

import informationModel.core.{edge, edgeJson, node, nodeJson, Dsl}

/**
 * Created by simonshapiro on 08/12/15.
 */
object modelDsl extends Dsl {
  def buildNode(n: nodeJson): node = {
    n._type match {
<#list nodes as node>
      case "${node.id()}" => {
        val s = ${node.id()}(n.id)
    <#if (node.propertiesJava()?size > 0)>
        n.properties.foreach(p => {
          p.name match {
        <#list node.propertiesJava() as prop>
            case "${prop.name()}" => s.${prop.name()}_(p.value)
         </#list>
            case _ => throw new IllegalArgumentException("%s:%s of type %s does not conform to dsl".format(p.name,p._type,p.value))
          }
        })
    </#if>
      s
      }
</#list>
      case _ => throw new IllegalArgumentException("%s:%s not in dsl".format(n.id, n._type))
    }
  }


  def buildEdge(e: edgeJson, nodes: List[node]): edge = {
    val fromNode = nodes.filter(n => (n.id == e.from)).head
    val toNode = nodes.filter(n => (n.id == e.to)).head
    e._type match {
  <#list edges as edge>
      case "${edge.from()}${edge.id()}${edge.to()}" => {
        val newEdge = new ${edge.from()}${edge.id()}${edge.to()}(fromNode.asInstanceOf[${edge.from()}],toNode.asInstanceOf[${edge.to()}],e.id)
        e.properties.foreach(p => {
          p.name match {
        <#list edge.propertiesJava() as prop>
            case "${prop.name()}" => newEdge.${prop.name()}_(p.value)
         </#list>
            case _ => throw new IllegalArgumentException("%s:%s of type %s does not conform to dsl".format(p.name,p._type,p.value))
          }
        })
        newEdge
      }
  </#list>
      case _ => throw new IllegalArgumentException("%s:%s not in dsl".format(e.id, e._type))
    }
  }
}
