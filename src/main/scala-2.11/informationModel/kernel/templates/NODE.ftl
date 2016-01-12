package informationModel.dsl

import informationModel.core.node
import scala.collection.mutable.ArrayBuffer

/**
 * Created by simonshapiro on dd/mm/yyyy.
 */

case class ${node().id()}(val uid: String = null) extends node {

  val id = if (uid != null) uid else uuid

  val _type: String = "${node().id()}"

<#if (node().propertiesJava()?size > 0)>
  <#list node().propertiesJava() as prop>
    private var _${prop.name()}: Option[${prop.valueType()}] = None
    def ${prop.name()} = _${prop.name()}
    def ${prop.name()}_(${prop.name()}:${prop.valueType()}) = {_${prop.name()} = Option(${prop.name()}) ; this}

  </#list>
</#if>

  def toJString: String = {
    val str = new ArrayBuffer[String]
    str += """ "id": "%s"""".format(id)
    str += """ "$type": "%s"""".format(_type)   // followed by an array of generalised properties (_name, _type, _valueString)
<#if (node().propertiesJava()?size > 0)>
  <#list node().propertiesJava() as prop>
        _${prop.name()} match {
          case Some(st) => str += """ "${prop.name()}": "%s"""".format(st)  //may need some shaping here around ${prop.valueType()}
          case None =>
        }

  </#list>
</#if>
    "{" + str.mkString(",") + "}"
  }

  def deepCopy: ${node().id()} = {
    val s = ${node().id()}(id)
<#if (node().propertiesJava()?size > 0)>
  <#list node().propertiesJava() as prop>
        _${prop.name()} match {
          case Some(x) => s.${prop.name()}_(x)
          case None =>
        }
  </#list>
</#if>
    s
  }

  override def isComplete: Boolean = true  // all properties optional

  override def isEqual(n: node) = {
    val d = n.asInstanceOf[${node().id()}]
    ((id == d.id)
<#if (node().propertiesJava()?size > 0)>
  <#list node().propertiesJava() as prop>
        && (${prop.name()}== d.${prop.name()})
  </#list>
</#if>
    )
  }

  def toDyNetMLAsJString: String = {
    val str = new ArrayBuffer[String]
    val propStr = new ArrayBuffer[String]
    str += """ "id": "%s"""".format(id)
    str += """ "$type": "%s"""".format(_type)
<#if (node().propertiesJava()?size > 0)>
  <#list node().propertiesJava() as prop>
        _${prop.name()} match {
          case Some(x) => propStr += propString[String]("${prop.name()}",x)
          case None =>
        }
  </#list>
</#if>
    str += """ "properties": [""" + propStr.mkString(",") + "]"
    "{" + str.mkString(",") + "}"
  }

<#list outboundEdges() as edge>
  def ${edge.id()}(s: ${edge.to()}, id: String = null) = {
    new ${edge.from()}${edge.id()}${edge.to()}(this, s, id)
  }
</#list>
}
