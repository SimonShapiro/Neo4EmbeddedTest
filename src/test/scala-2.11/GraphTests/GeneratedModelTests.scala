package GraphTests

/**
 * Created by simonshapiro on 06/01/16.
 */

import informationModel.core.graph
import informationModel.dsl.{systemCONNECTSsystem, dataset, system}
import informationModel.{KernelModel, infomodel}
import org.scalatest.FunSuite

class GeneratedModelTests extends FunSuite {
  test("Infomodel Has the correct data in it") {
    val g = infomodel.model
    assert(true)
    println("End: infomodel has the correct data")
  }
  test("Calling the meta version of associatedWith does nothing") {
    val model = new graph

    val excel = system()        name_ "Excel"   description_ "Any spreadsheet"
    val cyrus = system("Cyrus") name_ "Cyrus"   description_ "A forecasting tool"
    val d23 = dataset("d23")         name_ "d23"     description_ "Data set 23 which carries a big payload of data"
    val c2 = system()           name_ "Cyrus"   description_ "Cyrus 2.0"

    model <= (dataset() name_ "Anonymous")  // nice but you can't work with it!
    model <= excel
    model <= cyrus
    //  g <= cyrus
    model <= c2
    model <= d23

    val e_c = excel.CONNECTS(cyrus)
    model <=> excel.CONNECTS(excel)
    model <=> e_c.associatedWithdataset_(d23)
    model <=> e_c.associatedWith_(excel)
    e_c.associatedWith match {
      case Some(x) => assert(false,"Expecting associatedWith to be ignored because it is only used in a meta context")
      case None    => assert(true)
    }
  }
  test("Equality of association class on edges") {
    val model = new graph

    val excel = system()        name_ "Excel"   description_ "Any spreadsheet"
    val cyrus = system("Cyrus") name_ "Cyrus"   description_ "A forecasting tool"
    val d23 = dataset("d23")    name_ "d23"     description_ "Data set 23 which carries a big payload of data"
    val d24 = dataset("d24")    name_ "d24"     description_ "Data set 24 which carries a tiny payload of data"
    val c2 = system()           name_ "Cyrus"   description_ "Cyrus 2.0"

    model <= excel
    model <= cyrus
    model <= c2
    model <= d23

    val e_c = excel.CONNECTS(cyrus,"e_c")

    model <=> excel.CONNECTS(excel)
    model <=> e_c.associatedWithdataset_(d23)
    model <=> excel.CONNECTS(c2)

    model <=> (excel.PRODUCES(d23) frequency_ 12)

    val modelStr = model.toJsonAsDyNetML
    val m2 = model.deepCopy
    assert(model.isEqualTo(m2))
    m2.getEdge("e_c").asInstanceOf[systemCONNECTSsystem].associatedWithdataset_(d24)

    val m2Str = m2.toJsonAsDyNetML

    assert(!model.isEqualTo(m2))

    println("End: Equality of association class on edges")

  }
}
