package informationModel

import informationModel.core.graph
import informationModel.dsl.{system, dataset}

/**
 * Created by simonshapiro on 23/11/15.
 */
object infomodel extends App {
  val g = new graph

  val excel = system()        name_ "Excel"   description_ "Any spreadsheet"
  val cyrus = system("Cyrus") name_ "Cyrus"   description_ "A forecasting tool"
  val d23 = dataset()         name_ "d23"     description_ "Data set 23 which carries a big payload of data"
  val c2 = system()           name_ "Cyrus"   description_ "Cyrus 2.0"

  g <= (dataset() name_ "Anonymous")  // nice but you can't work with it!
  g <= excel
  g <= cyrus
//  g <= cyrus
  g <= c2
  println(c2.hashCode)
  g <= d23
  g <=> excel.CONNECTS(excel)
  g <=> excel.CONNECTS(cyrus)
  g <=> excel.CONNECTS(c2)
  g <=> (excel.PRODUCES(d23) frequency_ 12)
//  g <=> excel.PRODUCES(d23)

  val g2 = g.deepCopy
  g2.nodes("Cyrus").asInstanceOf[system].name_("Cyrus 2.0")

  val name = d23.name
//  val compundType = d23.getProperty("name").getClass.getCanonicalName
//  val nm = d23.getProperty("name")
  c2.description_("This should only change c2 and cause a problem")
  g.nodes.filter(n => n._2.isInstanceOf[system])
    .foreach(n => g.edges.filter(e => (e._2.from == n) || (e._2.to == n))
      .foreach(e => println(n.hashCode,e.getClass,e._2.to)))
}
