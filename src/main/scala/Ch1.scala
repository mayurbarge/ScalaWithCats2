enum Tree[A]:
  case Node(left: Tree[A], right: Tree[A])
  case Leaf(value: A)

  def size: Int =
    this match
      case Leaf(_) => 1
      case Node(left, right) => left.size + right.size
  def contains(elem: A): Boolean =
    this match
      case Leaf(value) => elem == value
      case Node(left, right) => left.contains(elem) || right.contains(elem)
  def map[B](f: A=>B): Tree[B] = // structural recursion (while matching) + corecursion (while creating Tree[B])
    this match
      case Leaf(value) => Leaf(f(value))
      case Node(left, right) => Node(left.map(f), right.map(f))

  def foldRight[B](z: B)(f: (A, B) => B): B = // abstract structural recursion as fold
    this match
      case Leaf(value) => z
      case Node(left, right) => left.foldRight(z)(f)

  def foldLeft[B](z: B)(f: (A, B) => B): B = // structural recursion
    println(s"Z: $z Current: $this")      // abstract structural recursion as fold
    this match
    case Leaf(value) => f(value, z)
    case Node(left, right) => right.foldLeft(left.foldLeft(z)(f))(f)


enum MyList[A]:
  case Empty()
  case Pair(head: A, tail: MyList[A])
  def map[B](f: A=> B): MyList[B] = // structural recursion + corecursion
    this match
      case Empty() => Empty()
      case Pair(head, tail) => Pair(f(head), tail.map(f))
  def size: Int =
    this match
      case Empty() => 0
      case Pair(_, tail) => 1 + tail.size

  def foldRight[B](z: B)(f: (A, B) => B): B = // abstract structural recursion as fold
    this match
      case Empty() => z
      case Pair(h, t) => f(h, t.foldRight(z)(f))
  def foldLeft[B](z: B)(f: (A, B) => B): B = // abstract structural recursion as fold
    println(s"Z: $z Current: $this")
    this match
      case Empty() => z
      case Pair(h, t) => t.foldLeft(f(h, z))(f)
  def foldLeft2[B](list: MyList[A], empty: B, f:(A, B)=> B): B =
    list match
      case Empty() => empty
      case Pair(h, t) => foldLeft2(t, f(h, empty), f)
object MyList {
  def unfold[A, B](seed: A)(stop: A => Boolean)(f: A => B)(next: A => A): MyList[B] = // corecursion
    if stop(seed) then MyList.Empty()
    else MyList.Pair(f(seed), unfold(next(seed))(stop)(f)(next))
  def fill[A](n: Int)(elem: => A): MyList[A] =
    unfold(n)(_ == 0)(_ => elem)(_ -1)
  def iterate[A](start: A, len: Int)(f: A => A): MyList[A] =
    unfold((len, start)){
      (len, _) => len == 0}
      {(_, start) => start}
      {(len, start) => (len - 1, f(start))
    }
}

object Ch1:
  @main
  def struturalRecusrion =
    import Tree.*
    val tree = Node(Node(Leaf(1),Leaf(2)), Node(Leaf(3),Leaf(4)))
    println(tree.foldLeft(0)(_ + _))

    import MyList.*
    val list: MyList[Int] = Pair(1, Pair(2, Pair(3,Empty())))
    println(list.foldLeft(0)(_ + _))
    println(MyList.iterate(1,10)(_+3))

