package informationModel

/**
 * Created by simonshapiro on 07/12/15.
 */
import play.api.libs.json._
import play.api.libs.functional.syntax._

object json extends App{
  val js = Json.obj(
    "one" -> "one",
    "twos" -> Json.arr(1 , 2, 3),
    "three" -> Json.arr(Json.obj(
      "name" -> "tester",
      "id" -> "tst",
      "age" -> 27
      ),Json.obj(
      "name" -> "test too",
      "id" -> "tst1",
      "age" -> 30
      )
    )
  )

  case class inner(name: String= null, id: String, age: Int)

  val customReader: Reads[List[Int]] = (__ \ "twos").read[List[Int]]
  val r = customReader.reads(js).fold(
    valid = { res =>
      val s: List[Int] = res
      s.foreach(i => println(i))
    },
    invalid = { errors => println(errors)}
  )

  implicit val innerReads: Reads[inner] = (
  (JsPath \ "name").read[String] and
  (JsPath \ "id").read[String]  and // readNullable
  (JsPath \ "age").read[Int]
)(inner.apply _)

  val res = (js \ "three").as[List[inner]]
  res.foreach(t => println(t.id))
  println("end...")
}

