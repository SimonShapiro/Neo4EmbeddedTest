package informationModel.core

import play.api.libs.json.Json

abstract class node extends graphMember {
  def deepCopy: node
  def hasSameId(n: node): Boolean = n.id == id
  def asJson = {
    Json.obj(
      "id" -> id,
      "type" -> getType,
      "properties" -> getPropertiesAsJsonObject)
  }
}

