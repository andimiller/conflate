import io.circe.Json.{JArray, JObject}
import io.circe.{ACursor, Decoder, Json}
import scala.collection.JavaConverters._

object Conflate extends App {
  import CirceHelpers.CirceMatchers._

  case class Conflated[T](d: EnvVariables => Decoder[T])

  case class EnvVariables(env: Map[String, String])

  object Conflated {
    implicit def decoderToConflated[T](implicit d: Decoder[T]) = {
      Conflated[T](
        (env: EnvVariables) =>
          Decoder.decodeJson.map{j =>
            traverse(j)(_.mapString( str =>
              env.env.toList.foldLeft(str) { case (s, (k, v)) => s.replaceAll("\\$"+k, v)}
            )).as[T].toOption.get
          }
      )
    }
  }

  private def traverse(c: ACursor)(f: Json => Json): ACursor = {
    c.focus.get match {
      case JObject(o) =>
        o.keys.foldLeft(c) { case (cursor, key) => traverse(cursor.downField(key))(f) }
      case JArray(a) =>
        (0 to a.length).foldLeft(c) { case (cursor, index) => traverse(cursor.downN(index))(f) }
      case _ =>
        c.withFocus(f).up
    }
  }

  private def traverse(j: Json)(f: Json => Json): Json = traverse(j.hcursor.asInstanceOf[ACursor])(f).top.get

  def apply[T](j: Json)(implicit env: EnvVariables, d: Conflated[T]): Decoder.Result[T] = {
    d.d(env).decodeJson(j)
  }
}
