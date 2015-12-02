/**
 * Created by simonshapiro on 26/11/15.
 */

import informationModel._
import informationModel.core.graph
import informationModel.dsl.{system, dataset}
import org.json4s.{NoTypeHints, native}
import org.scalatest.FunSuite

import scala.collection.mutable
import scala.native

class firstTests extends FunSuite {

  def setupTestGraph: graph = {
    val g = new graph
    val excel = system() //name="Excel", description="Any spreadsheet", age=12
            .<=("name","Excel")
            .<=("description","Any spreadsheet")
            .<=("age", 12)
    val cyrus = system("Cyrus") //name="Cyrus", description="A forecasting tool", age=1
            .<=("name","Cyrus")
            .<=("description","A forecasting tool")
            .<=("age", 1)
    val d23 = dataset()
            .<= ("name","d23")
            .<= ("description","Data set 23 which carries a big payload of data")
    val c2 = system()
            .<=("name","Cyrus")
            .<=("description","Cyrus 2.0")

    g <= dataset().<= (("name","Anonymous"))  // nice but you can't work with it!
    g <= excel
    g <= cyrus
    //  g <= cyrus
    g <= c2
    g <= d23
    g <=> excel.CONNECTS(excel)
    g <=> excel.CONNECTS(cyrus)
    g <=> excel.CONNECTS(c2)
    g <=> excel.PRODUCES(d23)
            .<= ("frequency", 12)
    g <=> excel.PRODUCES(d23)  //  arbitary duplicate edge
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
    assert(n.getClass.toString == "class informationModel.dsl.system")
    assert(e.id == "a->b")
    assert(e.getClass.toString == "class informationModel.dsl.systemCONNECTSsystem")
    println("End: A graph should be able to return a typed node or edge by id")
  }

  test("Both edge nodes need to be present in the node list of a graph") {
    val g = new graph
    g <=> system("fred").CONNECTS((system().<=("name","named only")))
    assert(g.nodes.size == 2)
    assert(g.edges.size == 1)
    println("End: Both edge nodes need to be present in the node list of a graph")
  }

  test("Two nodes are equal if they are match in all respects") {
    val a = system("fred").<=("name","jones").<=("age",27)
    val b = a.deepCopy
    val c = dataset()
    assert(a.hasSameId(b))
    assert(!a.hasSameId(c))
    assert(a.isIdenticalTo(b))
//    val p = b.getAllProperties
//    assert(a.hasEqualProperties(b.getAllProperties))
    b.<=("name","Jones")
    assert(!a.isIdenticalTo(b))
    println("End: Two nodes are equal if they are match in all respects")
  }

  test("Open manifest nodes are equal if they match in all respects - this should fix the same size for properties with different values problem wich was a feature of open nodes") {
    val a = dataset("a").<=("name","A data set")
    val b = a.deepCopy
    a.<=("age", 20)
    b.<=("years owned", 30)
    assert(!a.isIdenticalTo(b))
    println("End: Open manifest nodes are equal if they match in all respects")
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
    val sg = g.filterOnNodeType("dataset")
    assert((sg.nodes.size == 2) && (sg.edges.size == 1))
    assert(sg.isSubGraphOf(g))
    val n1 = sg.getNode("DS").asInstanceOf[dataset]
    n1.<=("name","ds")
    sg.getNode("S2").asInstanceOf[system].<=("name","fred")
    assert((!sg.isSubGraphOf(g)))
    val sg2 = g.filterOnNodeType("system")
    assert(sg2.isSubGraphOf(g))
    sg2 <=> sg2.getNode("S3").asInstanceOf[system].CONNECTS(s2)
    assert(!sg2.isSubGraphOf(g))
    println("End: A graph can return the sub-graph based on everything about a particular type of node")
  }

  test("graph should have a toJson string representation") {
    val g = new graph
    val s1 = system("S1").<=("name", "System 1").<=("age", 27)
    val s2 = system("S2")
    val ds = dataset("DS").<=("name", "Main Dataset")
    g <= s1
    g <= s2
    g <=> s1.CONNECTS(s2,"S1_S2")
    g <=> s2.PRODUCES(ds,"S2_DS").<=("costPerRun", 12.5)
/*
    s1.toJson
    s2.toJson
    */
    val expectedResult =
      """{"graph":{"nodes":[{"id":"DS","type":"dataset","properties":{"name":"Main Dataset"}},{"id":"S1","type":"system","properties":{"age":27,"name":"System 1"}},{"id":"S2","type":"system","properties":{}}],"edges":[{"id":"S2_DS","type":"systemPRODUCESdataset","from":"S2","to":"DS","properties":{"costPerRun":12.5}},{"id":"S1_S2","type":"systemCONNECTSsystem","from":"S1","to":"S2","properties":{}}]}}"""
    val json = g.toJson
    assert(json == expectedResult)
    println("End: graph should have a toJson string representation")
  }
}
