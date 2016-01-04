package GraphTests

import informationModel.KernelModel
import informationModel.core.{DslGenerator, GraphWriter}
import org.scalatest.FunSuite

import scala.collection.mutable

//import scala.reflect.io.File

/**
 * Created by simonshapiro on 13/12/15.
 */

import freemarker.template._
import java.util._
import java.io.{StringWriter, OutputStreamWriter, File}
import scala.collection.JavaConversions.asJavaDictionary


class KernelTest extends FunSuite {

  test("The kernal compiles because it succesfully saves as json file") {
    val path = "/Users/simonshapiro/IdeaProjects/Neo4EmbeddedTest/data/"
    val file = "Kernel"
    val g = KernelModel.model
    GraphWriter.writeFile(KernelModel.model,file,path)
    assert(true)
  }
  test("Template generator installed") {
    val cfg = new Configuration(Configuration.VERSION_2_3_23)
    cfg.setDirectoryForTemplateLoading(new File("/Users/simonshapiro/IdeaProjects/Neo4EmbeddedTest/src/main/scala-2.11/informationModel/kernel/templates"))
    cfg.setDefaultEncoding("UTF-8")
    cfg.setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER)
    val template = cfg.getTemplate("test.ftl")
// val out = new OutputStreamWriter(System.out)
    val out = new StringWriter
    val data = new scala.collection.mutable.HashMap[String, Any]
    data("name") = "fred"
    data("age") = 27
    val dataAsJava = asJavaDictionary(data)
    template.process(dataAsJava, out)
    val outString = out.toString
    assert(out.toString.slice(0,11) == "Hi fred(27)")
  }
  test("Generate dsl.scala from kernal model and ttemplate file") {
    DslGenerator.generateDsl("DSL.ftl", KernelModel.model)
    assert(false)
  }
}
