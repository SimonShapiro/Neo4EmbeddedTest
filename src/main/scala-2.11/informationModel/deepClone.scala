package informationModel

import java.io.{ObjectInputStream, ByteArrayInputStream, ObjectOutputStream, ByteArrayOutputStream}

/**
 * Created by simonshapiro on 30/11/15.
 */

object deepClone  {
  def clone(obj: Any): Any = {
    try {
      val c = obj.getClass
      val baos = new ByteArrayOutputStream
      val oos = new ObjectOutputStream(baos)
      oos.writeObject(obj)
      val bais = new ByteArrayInputStream(baos.toByteArray)
      val ois = new ObjectInputStream(bais)
      ois.readObject
    }
    catch {
      case e: Exception => {
      e.printStackTrace
      null
      }
    }
  }
}
