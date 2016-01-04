package GraphTests

/**
 * Created by simonshapiro on 26/11/15.
 */

import informationModel.core.{GraphReader, GraphWriter, graph}
import informationModel.dsl.{dataset, system, systemCONNECTSsystem}
import org.scalatest.FunSuite
import play.api.libs.json._

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
    val expectedResult = """{"graph":{"nodes":[{"id":"DS","$type":"Dataset","properties":[{"name":"name","type":"String","value":"Main Dataset"},{"name":"description","type":"String","value":"A description of the main dataset"}]},{"id":"S1","$type":"System","properties":[{"name":"name","type":"String","value":"System 1"}]},{"id":"S2","$type":"System","properties":[]}],"edges":[{"id":"S2_DS","$type":"SystemProducesDataset","from":"S2","to":"DS","properties":[{"name":"frequency","type":"Integer","value":"12"}]},{"id":"S1_S2","$type":"SystemConnectsSystem","from":"S1","to":"S2","properties":[]}]}}"""
    val json = g.toJsonAsDyNetML.toString
    println(json)
    assert(json == expectedResult)
    println("End: graph should have a toJsonDyNetML style string representation")
  }

  test("a graph should be built from a toJsonDyNetML string representation") {
    val g = new graph
    val s1 = system("S1").name_("System 1")
    val s2 = system("S2")
    val ds = dataset("DS").name_("Main Dataset").description_("A descritpion of the main dataset")
    g <= s1
    g <= s2
    g <=> s1.CONNECTS(s2,"S1_S2")
    g <=> s2.PRODUCES(ds,"S2_DS").frequency_(12)
    val json = g.toJsonAsDyNetML.toString
    val g2 = new graph(json)
    assert(g2.isEqualTo(g))
    println("End: graph should be built from a toJsonDyNetML string representation")
  }

  test("A graph should be persisted to a named directory.  " +
    "Each time it is persisted, it should be stored under a gmt time code") {
    val path = "/Users/simonshapiro/IdeaProjects/Neo4EmbeddedTest/data/"
    val gName = "mainGraph"

    val g = new graph
    val s1 = system("S1").name_("System 1")
    val s2 = system("S2")
    val ds = dataset("DS").name_("Main Dataset").description_("A descritpion of the main dataset")
    g <= s1
    g <= s2
    g <=> s1.CONNECTS(s2,"S1_S2")
    g <=> s2.PRODUCES(ds,"S2_DS").frequency_(12)

    val fullFileName = GraphWriter.writeFile(g, gName, path)
    val datetimePattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3}Z".r
//    val datetimePattern = "[0-9]{4}".r

    assert(fullFileName match {
      case Some (s) => true
      case None =>  false
    })

    val fn = fullFileName.get.split('/').last
    println(fn, fn.getClass)
    assert(datetimePattern.findFirstIn(fn) match {
      case Some(m) => true
      case None => false
    })

    println("End: file persistence")
  }

  test("Reconstitute a graph from a .dnml file") {
    val path = "/Users/simonshapiro/IdeaProjects/Neo4EmbeddedTest/data/"
    val gName = "mainGraph"

    val g = new graph
    val s1 = system("S1").name_("System 1")
    val s2 = system("S2")
    val ds = dataset("DS").name_("Main Dataset").description_("A descritpion of the main dataset")
    g <= s1
    g <= s2
    g <=> s1.CONNECTS(s2,"S1_S2")
    g <=> s2.PRODUCES(ds,"S2_DS").frequency_(12)

    val fullFileName = GraphWriter.writeFile(g, gName, path)
    val g2 = fullFileName match {
      case Some (s) => {
        println(s)
        GraphReader.readFile(gName, s.split('/').last, path)
      }
      case None => new graph()
    }
    assert(g2.isEqualTo(g))
    println("End: Reconstitute a graph from a .dnml file")
  }

  test("Two graphs can be merged together using g1.mergeInto(g2)") {
    val g1 = new graph()
    val g2 = new graph()
    g2 <= system("Two")
    val n1 = system("One")
    val n3 = system("Three")
    g1 <= n1
    g1 <= n3
    g1 <=> n1.CONNECTS(n3)

    g2 <=> n1.CONNECTS(n3)

    val ret1 = g1.merge(g2) match {
      case Right(x) => x
      case Left(x) =>  throw new IllegalStateException("Merge clashes:\n\t%s".format(x.mkString("\n")))
    }
    val g3 = ret1.asInstanceOf[graph]

    val ret2 = g2.merge(g1) match {
      case Right(x) => x
      case Left(x) =>  throw new IllegalStateException("Merge clashes:\n\t%s".format(x.mkString("\n")))
    }
    val g4 = ret2.asInstanceOf[graph]

    assert(g1.isSubGraphOf(g3))
    assert(g2.isSubGraphOf(g3))
    assert(g3.isEqualTo(g4))

    println("End: Two graphs can be merged together using g1.mergeInto(g2)")
  }

  test("Graphs retrieved from the persistance mechanism are independent, until merged") {

    val g1 = new graph()
    val g2 = new graph()
    g2 <= system("Two")
    val n1 = system("One")
    val n3 = system("Three")
    g1 <= n1
    g1 <= n3
    g1 <=> n1.CONNECTS(n3,"n1_n3")

    g2 <=> n1.CONNECTS(n3,"n1_n3")

    val path = "/Users/simonshapiro/IdeaProjects/Neo4EmbeddedTest/data/"
    val g1Name = "G1"
    val g2Name = "G2"
    
    val g1FileName = GraphWriter.writeFile(g1, g1Name, path)
    val g2FileName = GraphWriter.writeFile(g2, g2Name, path)
    val g1too = GraphReader.readFile(g1Name, g1FileName.get.split('/').last, path)
    val g2too = GraphReader.readFile(g2Name, g2FileName.get.split('/').last, path)

    g1too.getNode("Three").asInstanceOf[system].name_("fred")  // change the value of a single node in the g1too node "Three"
    g1too.getEdge("n1_n3").asInstanceOf[systemCONNECTSsystem].description_("n1 connects to n3 making it very interesting")

    assert(g1too.getNode("Three").asInstanceOf[system].name !=
           g2too.getNode("Three").asInstanceOf[system].name)
    assert(g2too.getNode("Three").asInstanceOf[system].name == None)

    val ret1 = g1too.merge(g2too)
    ret1 match {
      case Right(x) =>
      case Left(x) => {
        assert(x(0) == "Merge clash on node: Three")
        assert(x(1) == "Merge clash on edge: n1_n3")
      }
    }
    println("End: Graphs retrieved from the persistance mechanism are independent, until merged")
  }

  test("f.mergeWithAndUpdateBy(g) should have g overwrite f where appropriate") {
    val g1 = new graph()
    val g2 = new graph()
    g2 <= system("Two")
    val n1 = system("One")
    val n3 = system("Three")
    g1 <= n1
    g1 <= n3
    g1 <=> n1.CONNECTS(n3,"n1_n3")

    g2 <=> n1.CONNECTS(n3,"n1_n3")

    val path = "/Users/simonshapiro/IdeaProjects/Neo4EmbeddedTest/data/"
    val g1Name = "G1"
    val g2Name = "G2"

    val g1FileName = GraphWriter.writeFile(g1, g1Name, path)
    val g2FileName = GraphWriter.writeFile(g2, g2Name, path)
    val g1too = GraphReader.readFile(g1Name, g1FileName.get.split('/').last, path)
    val g2too = GraphReader.readFile(g2Name, g2FileName.get.split('/').last, path)

    g1too.getNode("Three").asInstanceOf[system].name_("fred")  // change the value of a single node in the g1too node "Three"
    g1too.getEdge("n1_n3").asInstanceOf[systemCONNECTSsystem].description_("n1 connects to n3 making it very interesting")
    val g3 = g2too.mergeWithAndUpdateBy(g1too)
    assert(g3.asInstanceOf[graph].getNode("Three").asInstanceOf[system].name == Some("fred"))
    println("End: f.mergeWithAndUpdateBy(g) should have g overwrite f where appropriate")
  }
}
