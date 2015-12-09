package informationModel.core

/**
 * Created by simonshapiro on 08/12/15.
 */
case class edgeJson(id: String,
                    $type: String,
                    from: String,
                    to: String,
                    properties: List[propJson])
