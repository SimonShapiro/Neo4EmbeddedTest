package informationModel.core

/**
 * Created by simonshapiro on 08/12/15.
 */
case class edgeJson(id: String,
                    _type: String,
                    from: String,
                    to: String,
                    associationNode: String,
                    properties: List[propJson])
