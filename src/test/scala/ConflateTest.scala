import Conflate.{Conflated, EnvVariables}
import io.circe.{Decoder, Json}
import org.scalatest.FlatSpec

class ConflateTest extends FlatSpec {
  case class Cat(name: String, age: Int)
  case class Person(name: String, age: Int, cats: List[Cat])

  implicit val catdec = Decoder.forProduct2("name", "age")(Cat.apply)

  "conflate" should "apply the transform" in {
    val c = Json.obj("name" -> Json.fromString("barry at $HOSTNAME"), "age" -> Json.fromInt(2))
    implicit val env = EnvVariables(Map("HOSTNAME" -> "testpc"))
    val result = Conflate[Cat](c)
    println(result)
  }
}
