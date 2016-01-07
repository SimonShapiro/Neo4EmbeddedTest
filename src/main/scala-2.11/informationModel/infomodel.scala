package informationModel

import informationModel.core.graph
import informationModel.dsl.{system, dataset}

/**
 * Created by simonshapiro on 23/11/15.
 */
object infomodel {
  val model = new graph

  val excel = system()        name_ "Excel"   description_ "Any spreadsheet"
  val cyrus = system("Cyrus") name_ "Cyrus"   description_ "A forecasting tool"
  val d23 = dataset("d23")    name_ "d23"     description_ "Data set 23 which carries a big payload of data"
  val c2 = system()           name_ "Cyrus"   description_ "Cyrus 2.0"

  model <= excel
  model <= cyrus
  model <= c2
  model <= d23

  model <=> excel.CONNECTS(excel)
  model <=> excel.CONNECTS(cyrus).associatedWithDataset_(d23)
  model <=> excel.CONNECTS(c2)

  model <=> (excel.PRODUCES(d23) frequency_ 12)

}
