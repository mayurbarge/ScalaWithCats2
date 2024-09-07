
object Ch4:
  object A {
    given a: Int = 1
    //given b: Int = 2 ambiguous given instances error
    object B {
      C.whichInt
    }

    object C {
      def whichInt(using i: Int): Int = i
    }

    def whichInt(using i: Int): Int = i
  }
  object B {
    import A.{given, *}
    whichInt
  }

  trait Sound[A]:
    def sound: String
  object Sound:
    def soundOf[A](using s: Sound[A]): String =
      s.sound
  trait Cat
  object Cat:
    given catSound: Sound[Cat] =
      new Sound[Cat]:
        override def sound: String = s"meow"
  trait Dog
  object Dog:
    given dogSound: Sound[Dog] =
      new Sound[Dog]:
        override def sound: String = s"woof"
  @main
  def contextualAbstraction =
    println(Sound.soundOf[Cat]) // both companion objects of Sound and A are in scope
    println(Sound.soundOf[Dog])
    println(A.whichInt(using 10)) // highest priority - explicit instance passing

    given purr: Sound[Cat] = new Sound[Cat]:
      override def sound: String = s"purr"
    println(Sound.soundOf[Cat]) // instances in lexical scope second highest priority
    { // instances in closest lexical scope take priority than farther away
      given growl: Sound[Dog] = new Sound[Dog]:
        override def sound: String = s"growl"
      given mew: Sound[Cat] = new Sound[Cat]:
        override def sound: String = "mew"
      println(Sound.soundOf[Dog])
      println(Sound.soundOf[Cat])
    }





