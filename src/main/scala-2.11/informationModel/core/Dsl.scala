package informationModel.core

/**
 * Created by simonshapiro on 05/01/16.
 */
abstract class Dsl {
  def buildNode(n: nodeJson): node
  def buildEdge(e: edgeJson, nodes: List[node]): edge
}
