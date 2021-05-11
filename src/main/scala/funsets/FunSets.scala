package funsets

object FunSets {

  /**
    * The bounds for `forall` and `exists` are +/- 1000.
    */
  val limit = 1000

  /**
    * We represent a set by its characteristic function, i.e.
    * its `contains` predicate.
    */
  type Set = Int => Boolean

  /**
    * Indicates whether a set contains a given element.
    */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
    * Returns the set of the one given element.
    */
  def singletonSet(elem: Int): Set = x => x == elem

  /**
    * Returns the union of the two given sets,
    * the sets of all elements that are in either `s` or `t`.
    */
  def union(a: Set, b: Set): Set = x => a(x) || b(x)

  /**
    * Returns the intersection of the two given sets,
    * the set of all elements that are both in `s` and `t`.
    */
  def intersect(a: Set, b: Set): Set = x => a(x) && b(x)

  /**
    * Returns the difference of the two given sets,
    * the set of all elements of `s` that are not in `t`.
    */
  def diff(s: Set, t: Set): Set = x => s(x) && !t(x)

  /**
    * Returns the subset of `s` for which `p` holds.
    */
  def filter(a: Set, p: Int => Boolean): Set = intersect(a, p)

  /**
    * Returns whether all bounded integers within `s` satisfy `p`.
    */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def inforall(el: Int): Boolean =
      if (el > limit) {
        true
      } else if (contains(s, el) && p(el)) {
        inforall(el + 1)
      } else {
        false
      }
    inforall(-limit)
  }

  /**
    * Returns whether there exists a bounded integer within `s`
    * that satisfies `p`.
    */
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))

  /**
    * Returns a set transformed by applying `f` to each element of `s`.
    */
  def map(a: Set, f: Int => Int): Set = x => exists(a, y => f(y) == x)

  /**
    * Displays the contents of a set
    */
  def toString(s: Set): String = {
    val xs = for (i <- -limit to limit if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
    * Prints the contents of a set on the console.
    */
  def printSet(s: Set) {
    println(toString(s))
  }
}
