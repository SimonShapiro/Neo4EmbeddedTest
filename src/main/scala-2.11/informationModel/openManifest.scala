package informationModel

/**
 * Created by simonshapiro on 25/11/15.
 */
import scala.collection.mutable

trait openManifest extends  manifest {

  def setProperty(k: String, v: Any) = {
    if (manifest.contains(k)) {
      val keyType = manifest(k)._1
      val valueType = v.getClass.getCanonicalName
      if (keyType == valueType) properties(k) = v
      else throw new IllegalArgumentException(""" Expecting type "%s" received type "%s" for "%s" """.format(keyType,valueType,k))
    } // test for type as well
    else properties(k) = v
    this
  }

}
