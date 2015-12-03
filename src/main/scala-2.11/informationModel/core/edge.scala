package informationModel.core

import informationModel.core.node
import play.api.libs.json.Json

/**
 * Created by simonshapiro on 23/11/15.
 */
abstract class edge (val from: node,val to: node) extends graphMember{
  def deepCopy: edge
  def hasSameId(e: edge): Boolean = e.id == id
  def asJson = {
    Json.obj(
      "id"    -> id,
      "type"  -> getType,
      "from"  -> from.id,
      "to"    -> to.id,
      "properties" -> getPropertiesAsJsonObject)
  }
}
