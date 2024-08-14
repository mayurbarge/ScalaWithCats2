
sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
case object JsNull extends Json

object Json: // typeclass use with interface object
  def toJson[A](value: A)(jsonWriter: JsonWriter[A]): Json =
    jsonWriter.write(value)
object JsonSyntax:
  extension[A](value:A) // typeclass use with interface syntax
    def toJson(using w: JsonWriter[A]): Json =
      w.write(value)
trait JsonWriter[A]: // typeclass
  def write(value: A): Json

object JsonWriterInstances:
  given stringWriter: JsonWriter[String] =
    new JsonWriter[String]:
      override def write(value: String): Json = JsString(value)
  final case class Person(name: String, email: String)
  given JsonWriter[Person] with // anonymous given instance, use of with
    def write(value: Person): Json = JsObject(Map(
        "name" -> JsString(value.name),
        "email" -> JsString(value.email)
      ))

  given optionWriter[A](using writer: JsonWriter[A]): JsonWriter[Option[A]] =
    new JsonWriter[Option[A]]:
      override def write(value: Option[A]): Json =
        value match
          case None => JsNull
          case Some(aValue) => writer.write(aValue)

  trait Display[T]: // type class with T
    def display(value: T): String
  object Display:
    extension [A](value: A) // interface syntax
      def print(using display: Display[A]): Unit = // type class use: using
        println(display.display(value))

    extension [A](value: A)
      def display(using display: Display[A]): String =
        display.display(value)
    given stringDisplay: Display[String] with // typeclass instance : given
      override def display(value: String): String = value
    given intDisplay: Display[Int] with
      override def display(value: Int): String = value.toString

trait Animal
trait Cat extends Animal
trait DomesticShorthair extends Cat

trait Inv[A]:
  def result: String
object Inv:
  given Inv[Cat] with
    def result = "Invariant"
  def apply[A](using instance: Inv[A]): String =
    instance.result
trait Co[+A]:
  def result: String
object Co:
  given Co[Cat] with
    override def result: String = "Covariant"
  def apply[A](using instance: Co[A]): String =
    instance.result
trait Contra[-A]:
  def result: String
object Contra:
  given Contra[Cat] with
    override def result: String = "Contravariant"
  def apply[A](using instance: Contra[A]): String =
    instance.result
object Ch4v2:
  @main
  def typeclass =
    import JsonWriterInstances.{given, *}
    println(Json.toJson(Person("Dave", "dave@example.com")) )

    import JsonWriterInstances.given
    import JsonSyntax.*
    Person("Dave", "dave@example.com").toJson

    summon[JsonWriter[String]] // for debugging

    import Display.{given, *}
    Display.print(10)
    println(Display.display("Test"))

    println("-------------------")
    println(Inv[Cat])
    println(Co[Animal])
    println(Co[Cat])
    println(Contra[DomesticShorthair])
    println(Contra[Cat])