import Stream.ones

trait Set[A]: // codata
  def contains(elt: A): Boolean
  def insert(elt: A): Set[A]
  def union(that: Set[A]): Set[A] =
    val self = this
    new Set[A] {
      def contains(elt: A): Boolean = self.contains(elt) || that.contains(elt)
      def insert(elt: A): Set[A] = self.insert(elt).union(that)
    }

final class ListSet[A](elements: List[A]) extends Set[A]: // codata
  override def contains(elt: A): Boolean = elements.contains(elt)

  override def insert(elt: A): Set[A] = ListSet(elt :: elements)

  override def union(that: Set[A]): Set[A] = elements.foldLeft(that){(set, elt) => set.insert(elt)}
object ListSet:
  def empt[A]: Set[A] = ListSet(List.empty)

trait Stream[A]:
  def head: A
  def tail: Stream[A]

  def take(count: Int): List[A]=
    count match
      case 0 => Nil
      case n => head :: tail.take(n-1)
  def map[B](f: A => B): Stream[B] =
    val self = this // renaming is necessary, if we call this inside new Stream it will refer new object
    new Stream[B] {
      override def head: B = f(self.head)

      override def tail: Stream[B] = self.tail.map(f)
      }

enum ListData[A]: // ADT or Data
  case Empty()
  case Pair(head: A, tail: ListData[A])

trait ListCodata[A]: // codata
  def empty: A
  def pair(head:A, tail:ListCodata[A]): ListCodata[A]

enum Bool2:
  case True
  case False
  def fold[A](t: A)(f: A): A =
    this match
      case True => f
      case False => f
trait Bool:
  def `if`[A](t: A)(f: A): A
object Bool:
  val True = new Bool:
    override def `if`[A](t: A)(f: A): A = t
  val False = new Bool:
    override def `if`[A](t: A)(f: A): A = f
  def and(l: Bool, r: Bool): Bool =
    new Bool:
      override def `if`[A](t: A)(f: A): A =
        l.`if`(r)(False).`if`(t)(f)
        // l.if(r) may give true / false and its first argument of if i.e.(t:A)
        // second argument is (f: A) hence False is passed
        // last section determines what to show based on this computation

object Stream:
  val ones: Stream[Int] = new Stream[Int] {
    override def head: Int = 1

    override def tail: Stream[Int] = ones
  }
  def unfold[A, B](seed: A)(f: A => B)(next: A=> A): Stream[B] =
    new Stream[B]:
      override def head: B = f(seed)

      override def tail: Stream[B] = unfold(next(seed))(f)(next)


object Ch3:
  @main
  def codata =
    import Stream.*
    println(Stream.ones.head)
    println(Stream.ones.tail.head)
    println(Stream.ones.take(4))

    val alternating = Stream.unfold(true)(x => if x then 1 else -1)(x => !x)
    println(alternating.take(5))

    import Bool.*
    println(and(True, False).`if`("true")("false"))
    println(and(True, True).`if`("true")("false"))

