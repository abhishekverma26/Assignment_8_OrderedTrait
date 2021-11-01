trait Sets[A <: Ordered[A]] {
  def incl(v: A): Sets[A]
  def contains(v: A): Boolean
}

class EmptySet[A <: Ordered[A]] extends Sets[A] {
  def incl(v: A): Sets[A] = new NonEmptySet(v, new EmptySet[A], new EmptySet[A])
  def contains(v: A): Boolean = false
}

class NonEmptySet[A <: Ordered[A]](element: A, left: Sets[A], right: Sets[A]) extends Sets[A] {
  def contains(v: A): Boolean = v match {
    case a if a < element => left contains a
    case b if b > element => right contains b
    case _ => true
  }

  def incl(v: A): Sets[A] = v match {
    case a if a < element => new NonEmptySet(element, left incl a, right)
    case b if b > element => new NonEmptySet(element, left, right incl b)
    case _ => this
  }
}

case class NewSet(element: Int) extends Ordered[NewSet] {
  def compare(that: NewSet): Int = {
    if (this.element < that.element) -1
    else if (this.element > that.element) 1
    else 0
  }
}