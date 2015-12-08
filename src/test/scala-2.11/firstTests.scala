/**
 * Created by simonshapiro on 26/11/15.
 */

import informationModel._
import informationModel.core.graph
import informationModel.dsl.{system, dataset}
import org.scalatest.FunSuite
import play.api.libs.json._
import play.api.libs.functional.syntax._

import scala.collection.mutable
import scala.native

class firstTests extends FunSuite {

  def setupTestGraph: graph = {
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
    g
  }

  test("True") {
    setupTestGraph
    assert(true)
  }
  test("A graph consists of a map of (concrete) nodes and edges") {
    val g = new graph
    g <= system()
    assert(g.nodes.size == 1)
    assert(g.edges.size == 0)
    assert(g.nodes.head._2.id != null)
    assert(g.nodes.head._2.id.length == 36)  // generates an guid of a known length
    val g2 = new graph
    g2 <= system("fred_001")
    assert(g2.nodes.head._2.id == "fred_001")
    println("End: basic graph test")
  }

  test("A graph should be able to return a typed node or edge by id") {
    val g = new graph
    val a = system("a")
    val b = system("b")
    g <= a
    g <= b
    g <=> a.CONNECTS(b,"a->b")
    val n = g.getNode("a")
    val e = g.getEdge("a->b")
    assert(n.id == "a")
    assert(n.getType == "System")
    assert(e.id == "a->b")
    assert(e.getType == "SystemConnectsSystem")
    println("End: A graph should be able to return a typed node or edge by id")
  }

  test("Both edge nodes need to be present in the node list of a graph") {
    val g = new graph
    g <=> (system("fred").CONNECTS(system().name_("named only")))
    assert(g.nodes.size == 2)
    assert(g.edges.size == 1)
    println("End: Both edge nodes need to be present in the node list of a graph")
  }
  test("Two nodes are equal if they are match in all respects") {
    val a = system("fred") name_ "jones"
    val b = a.deepCopy
    val c = dataset()
    assert(a.isEqual(b))
    //      assert(!a.isEqual(c))
    //      assert(a.isIdenticalTo(b))
    //    val p = b.getAllProperties
    //    assert(a.hasEqualProperties(b.getAllProperties))
    b.name_("Jones")
    assert(!a.isEqual(b))
    println("End: Two nodes are equal if they are match in all respects")
  }
  test("Two nodes are equal if they match in Json text") {
    val a = system("fred") name_ "jones"
    val b = a.deepCopy
    val c = dataset()
    assert(a.isJsonEqual(b))
    assert(!a.isJsonEqual(c))
    //      assert(a.isIdenticalTo(b))
    //    val p = b.getAllProperties
    //    assert(a.hasEqualProperties(b.getAllProperties))
    b.name_("Jones")
    assert(!a.isJsonEqual(b))
    println("End: Two nodes are equal if they match in Json text")
  }
    test("A graph can return the sub-graph based on everything about a particular type of node") {
      val g = new graph
      val s1 = system("S1")
      val s2 = system("S2")
      val s3 = system("S3")
      val ds = dataset("DS")
      g <= s1
      g <= s2
      g <= s3
      g <=> s1.CONNECTS(s2)
      g <=> s2.CONNECTS(s3)
      g <=> s3.CONNECTS(s1)
      g <=> s2.PRODUCES(ds)
      /*
      val sg = g.filterEdges(systemPRODUCESdataset)
      assert(sg.nodes.size == 1)
      assert(sg.nodes.head.id == "S2")
      assert(sg.isSubGraphOf(g))
  //    gUtils.typedSubgraph[system](g)
      */
      val sg = g.filterOnNodeType("Dataset")
      assert((sg.nodes.size == 2) && (sg.edges.size == 1))
      assert(sg.isSubGraphOf(g))
      sg.getNode("DS").asInstanceOf[dataset].name_("ds")
      sg.getNode("S2").asInstanceOf[system].name_("fred")
      assert((!sg.isSubGraphOf(g)))
      val sg2 = g.filterOnNodeType("System")
      assert(sg2.isSubGraphOf(g))
      sg2 <=> sg2.getNode("S3").asInstanceOf[system].CONNECTS(s2)
      assert(!sg2.isSubGraphOf(g))
      println("End: A graph can return the sub-graph based on everything about a particular type of node")
    }

  test("graph should have a toJson string representation") {
    val g = new graph
    val s1 = system("S1").name_("System 1")
    val s2 = system("S2")
    val ds = dataset("DS").name_("Main Dataset")
    g <= s1
    g <= s2
    g <=> s1.CONNECTS(s2,"S1_S2")
    g <=> s2.PRODUCES(ds,"S2_DS")
    val expectedResult = """{"graph":{"nodes":[{"id":"DS","$type":"Dataset","name":"Main Dataset"},{"id":"S1","$type":"System","name":"System 1"},{"id":"S2","$type":"System"}],"edges":[{"id":"S2_DS","$type":"SystemProducesDataset","from":"S2","to":"DS"},{"id":"S1_S2","$type":"SystemConnectsSystem","from":"S1","to":"S2"}]}}"""
    val json = g.toJson
    assert(json == expectedResult)
    val jsonObj = Json.parse(json)
    val nodeObj = jsonObj \ "graph" \ "nodes"
    println("End: graph should have a toJson string representation")
  }

  test("graph should have a toJsonDyNetML style string representation") {
    val g = new graph
    val s1 = system("S1").name_("System 1")
    val s2 = system("S2")
    val ds = dataset("DS").name_("Main Dataset").description_("A description of the main dataset")
    g <= s1
    g <= s2
    g <=> s1.CONNECTS(s2,"S1_S2")
    g <=> s2.PRODUCES(ds,"S2_DS").frequency_(12)
    val expectedResult = """{"graph":{"nodes":[{"id":"DS","$type":"Dataset","properties":[{"name":"name","type":"String","value":"Main Dataset"},{"name":"description","type":"String","value":"A description of the main dataset"}]},{"id":"S1","$type":"System","properties":[{"name":"name","type":"String","value":"System 1"}]},{"id":"S2","$type":"System","properties":[]}],"edges":[{"id":"S2_DS","$type":"SystemProducesDataset","from":"S2","to":"DS","properties":[{"name":"name","type":"Integer","value":"12"}]},{"id":"S1_S2","$type":"SystemConnectsSystem","from":"S1","to":"S2","properties":[]}]}}"""
    val json = g.toJsonAsDynetML
    println(json)
    assert(json == expectedResult)
    println("End: graph should have a toJsonDyNetML style string representation")
  }

  test("a graph should be built from a Json string representation") {
    val g = new graph
    val s1 = system("S1").name_("System 1")
    val s2 = system("S2")
    val ds = dataset("DS").name_("Main Dataset").description_("A descritpion of the main dataset")
    g <= s1
    g <= s2
    g <=> s1.CONNECTS(s2,"S1_S2")
    g <=> s2.PRODUCES(ds,"S2_DS").frequency_(12)
    val json = g.toJsonAsDynetML
    val g2 = new graph(json)
    assert(g2.isEqualTo(g))
  }
}
