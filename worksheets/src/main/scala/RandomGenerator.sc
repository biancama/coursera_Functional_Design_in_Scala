import scala.util.Random

trait Generator[+T] {

  def generate: T
}

val integers = new Generator[Integer] {
  val i = new Random()

  override def generate = i.nextInt()
}


val booleans = new Generator[Boolean] {
  override def generate = integers.generate > 0
}

val pairs = new Generator[(Int, Int)] {
  override def generate = (integers.generate, integers.generate)
}

trait GenericGenerator[+T] {
  self => // an alisr for this
  def generate: T
  def map[S] (f: T => S): GenericGenerator[S] = new GenericGenerator[S] {
    override def generate = f(self.generate)
    //override def generate = f(GenericGenerator.this.generate)
  }

  def flatMap[S](f: T => GenericGenerator[S]): GenericGenerator[S] = new GenericGenerator[S] {
    override def generate = f(self.generate).generate
  }


}

val integersGeneric = new GenericGenerator[Int] {
  val i = new Random()
  override def generate = i.nextInt()
}

val booleansGeneric = new GenericGenerator[Boolean] {
  override def generate = integersGeneric.generate > 0
}

def single[T](x: T): GenericGenerator[T] = new GenericGenerator[T] {
  override def generate = x
}

def choose(lo: Int, hi: Int): GenericGenerator[Int] = for (x <- integersGeneric) yield lo + x %(hi - lo)

def oneOf[T] (xs: T*) : GenericGenerator[T] = for (idx <- choose(0, xs.length)) yield xs (idx)

def emptyList = single(Nil)
def nonEmptyList = for {
  head <- integersGeneric
  tail <- lists
} yield head::tail

def lists: GenericGenerator[List[Int]] = for {
  isEmpty <- booleansGeneric
  list <- if (isEmpty) emptyList else nonEmptyList
} yield list


trait Tree
case class Inner(left: Tree, right: Tree) extends Tree
case class Leaf(x: Int) extends Tree

def leafs = for (i <- integersGeneric) yield Leaf(i)
def inners = for {
  l <- trees
  r <- trees
} yield (l, r)

def trees : GenericGenerator[Tree] = for {
  isLeaf <- booleansGeneric.generate
  tree <- if (isLeaf) leafs else inners
} yield tree
