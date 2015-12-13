package GraphTests

import informationModel.KernelModel
import informationModel.core.GraphWriter
import org.scalatest.FunSuite

/**
 * Created by simonshapiro on 13/12/15.
 */
class KernelTest extends FunSuite {

  test("The kernal compiles") {
    val path = "/Users/simonshapiro/IdeaProjects/Neo4EmbeddedTest/data/"
    val file = "Kernel"
    val g = KernelModel.meta1
    GraphWriter.writeFile(KernelModel.meta1,file,path)
    assert(true)
  }
}
