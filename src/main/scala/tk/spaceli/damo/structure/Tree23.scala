package tk.spaceli.damo.structure

/**
  * Created by ucfawli on 04-Aug-16.
  */

trait Tree23[T] {
  def insert(x: T): Either[Tree23[T], (T, Tree23[T], Tree23[T])]
  def search(x: T): Option[T]
}

case class Empty[T: Ordering]() extends Tree23[T] {
  override def insert(x: T): Either[Tree23[T], (T, Tree23[T], Tree23[T])] = {
    Left(Tree23V(x))
  }

  override def search(x: T): Option[T] = None
}

case class Tree23Root[T:Ordering](root: Tree23[T]) {
  def insert(x: T): Tree23Root[T] = Tree23Root(root.insert(x) match {
    case Left(t) => t
    case Right((n, l, r)) => Tree23VLL(n, l, r)
  })

  def search(x: T): Option[T] = root search x
}

case class Tree23V[T : Ordering](a: T) extends Tree23[T] {
  val ord = implicitly[Ordering[T]]
  import ord._
  override def insert(x: T): Either[Tree23[T], (T, Tree23[T], Tree23[T])] = {
    if (a < x)
      Left(Tree23VV(a, x))
    else
      Left(Tree23VV(x, a))
  }

  override def search(x: T): Option[T] = if (x == a) Some(x) else None
}

case class Tree23VV[T: Ordering](a: T, b: T) extends Tree23[T] {
  val ord = implicitly[Ordering[T]]
  import ord._
  override def insert(x: T): Either[Tree23[T], (T, Tree23[T], Tree23[T])] = {
    if (x < a)
      Right((a, Tree23V(x), Tree23V(b)))
    else if (x < b)
      Right((x, Tree23V(a), Tree23V(b)))
    else
      Right((b, Tree23V(a), Tree23V(x)))
  }

  override def search(x: T): Option[T] = if (x == a && x == b) Some(x) else None
}

case class Tree23VLL[T: Ordering](a: T, ltree: Tree23[T], rtree: Tree23[T]) extends Tree23[T] {
  val ord = implicitly[Ordering[T]]
  import ord._
  override def insert(x: T): Either[Tree23[T], (T, Tree23[T], Tree23[T])] = {
    if (x < a) {
      ltree.insert(x) match {
        case Left(l) => Left(Tree23VLL(a, l, rtree))
        case Right((y, l, r)) => Left(Tree23VVLLL(y, a, l, r, rtree))
      }
    } else {
      rtree.insert(x) match {
        case Left(r) => Left(Tree23VLL(a, ltree, r))
        case Right((y, l, r)) => Left(Tree23VVLLL(a, y, ltree, l, r))
      }
    }
  }

  override def search(x: T): Option[T] = if (x == a) Some(x) else if (x < a) ltree.search(x) else rtree.search(x)
}

case class Tree23VVLLL[T: Ordering](a: T, b: T, ltree: Tree23[T], mtree: Tree23[T], rtree: Tree23[T]) extends Tree23[T] {
  val ord = implicitly[Ordering[T]]
  import ord._

  override def insert(x: T): Either[Tree23[T], (T, Tree23[T], Tree23[T])] = {
    if (x < a) {
      ltree.insert(x) match {
        case Left(l) => Left(Tree23VVLLL(a, b, l, mtree, rtree))
        case Right((y, l, r)) => Right(a, Tree23VLL(y, l, r), Tree23VLL(b, mtree, rtree))
      }
    } else if (x < b){
      mtree.insert(x) match {
        case Left(m) => Left(Tree23VVLLL(a, b, ltree, m, rtree))
        case Right((y, l, r)) => Right(y, Tree23VLL(a, ltree, l), Tree23VLL(b, r, rtree))
      }
    } else {
      rtree.insert(x) match {
        case Left(r) => Left(Tree23VVLLL(a, b, ltree, mtree, r))
        case Right((y, l, r)) => Right(b, Tree23VLL(a, ltree, mtree), Tree23VLL(b, l, r))
      }
    }
  }

  override def search(x: T): Option[T] =
    if (x == a && x == b) Some(x)
    else if (x < a) ltree.search(x)
    else if (x < b) mtree.search(x)
    else rtree.search(x)
}
