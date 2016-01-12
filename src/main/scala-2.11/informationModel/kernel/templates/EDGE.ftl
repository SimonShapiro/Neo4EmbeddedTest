package informationModel.dsl

import informationModel.core.{node, edge}
import scala.collection.mutable.ArrayBuffer

/**
 * Created by simonshapiro on 23/11/15.
 */
class ${from()}${id()}${to()}(from: ${from()}, to: ${to()}, uid: String = null) extends edge(from, to) {

  val id = if (uid != null) uid else uuid

  val _type: String = "${from()}${id()}${to()}"

<#list propertiesJava() as prop>
  <#switch prop.valueType()>
    <#case "AssociationClass">
        override def associatedWith_(associationNode: node) = this  // to prevent the misuse of associatedWith_
      
        private var _associatedWith${prop.name()}: Option[${prop.name()}] = None
        def associatedWith${prop.name()} = _associatedWith${prop.name()}
        def associatedWith${prop.name()}_(associationNode: ${prop.name()}) = {
                                                _associatedWith${prop.name()} = Option(associationNode)
                                                this
                                              }
        private def associatedWith${prop.name()}Equals(a: Option[${prop.name()}], b: Option[${prop.name()}]): Boolean = {
          a match {
            case Some(x) => b match {
              case Some(y) => x.isEqual(y)
              case None    => false
            }
            case None =>  b match {
              case Some (y) => false
              case None     => true
            }
          }
        }
      <#break>
    <#case "Integer">
      private var _${prop.name()}: Option[Int] = None
      def ${prop.name()} = _${prop.name()}
      def ${prop.name()}_(${prop.name()}: Int) = {_${prop.name()} = Option(${prop.name()}) ; this}
      <#break>
    <#default>
      private var _${prop.name()}: Option[String] = None
      def ${prop.name()} = _${prop.name()}
      def ${prop.name()}_(${prop.name()}: String) = {_${prop.name()} = Option(${prop.name()}) ; this}
      <#break>
  </#switch>
</#list>

  def toJString: String = {
    val str = header
<#list propertiesJava() as prop>
  <#switch prop.valueType()>
    <#case "Integer">
        _${prop.name()} match {
          case Some(st) => str += """ "${prop.name()}": %s""".format(st)  //may need some shaping here around ${prop.valueType()}
          case None =>
        }
    <#break>
    <#case "AssociationClass">
      associatedWith${prop.name()} match {
            case Some(x) => str += """ "associatedWith${prop.name()}": "%s" """.format(x.id)
            case None =>
      }
    <#break>
    <#default>
        _${prop.name()} match {
          case Some(st) => str += """ "${prop.name()}": "%s"""".format(st)  //may need some shaping here around ${prop.valueType()}
          case None =>
        }
    <#break>
  </#switch>
  </#list>
    "{" + str.mkString(",") + "}"
  }

  def deepCopy: ${from()}${id()}${to()} = {   //  strangely not copying the association - could be a problem
    val e = new ${from()}${id()}${to()}(from, to, id)

<#list propertiesJava() as prop>
  <#if (prop.valueType() != "AssociationClass")>
    _${prop.name()} match {
      case Some(f) => e.${prop.name()}_(f)
      case None =>
    }
  <#else>
  // placeholder for assoc class copy
  </#if>
</#list>
    e
  }

  override def isEqual(ee: edge) = {
    val e = ee.asInstanceOf[${from()}${id()}${to()}]
    ((id == e.id)
    && (from == e.from)
    && (to == e.to)
  <#list propertiesJava() as prop>
    <#if (prop.valueType() != "AssociationClass")>
      && (${prop.name()} == e.${prop.name()})
    <#else>
      && associatedWith${prop.name()}Equals(associatedWith${prop.name()},e.associatedWith${prop.name()})
    </#if>
  </#list>
    )
  }

  override def isComplete: Boolean = true

  def toDyNetMLAsJString: String = {
    val str = header
    val propStr = new ArrayBuffer[String]
<#list propertiesJava() as prop>
  <#switch prop.valueType()>
    <#case "Integer">
      _${prop.name()} match {
      case Some(i) => propStr += propString[Int]("${prop.name()}",i)
      case None =>
      }
    <#break>
    <#case "AssociationClass">
      _associatedWith${prop.name()} match {
      case Some(i) => propStr += propString[${prop.name()}]("associatedWith${prop.name()}",i)
      case None =>
      }
    <#break>
    <#default>
      _${prop.name()} match {
      case Some(i) => propStr += propString[String]("${prop.name()}",i)
      case None =>
      }
    <#break>
  </#switch>
</#list>
    str += """ "properties": [""" + propStr.mkString(",") + "]"
    "{" + str.mkString(",") + "}"
  }
}
