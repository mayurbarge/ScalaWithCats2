import cats.*
import cats.syntax.all.*
object Ch6:
  @main
  def usingCats =
    val showInt = Show.apply[Int]
    println(showInt.show(10))
    println(40.show)
    import java.util.Date
    given showDate: Show[Date] with
      override def show(t: Date): String = s"${t.getTime}ms after the epoch."

    println(new Date().show)
