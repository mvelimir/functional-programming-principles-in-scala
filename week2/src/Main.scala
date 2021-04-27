

object Main {
  
  val limit = 1000
  
  type Set = Int => Boolean
  
  def contains(s: Set, elem: Int): Boolean = s(elem)
  
  def singletonSet(elem: Int): Set = x => x == elem
  
  def union(a: Set, b: Set): Set = x => a(x) || b(x)
  
  def intersect(a: Set, b: Set): Set = x => a(x) && b(x)
  
  def diff(s: Set, t: Set): Set = x => s(x) && !t(x)
  
  def filter(a: Set, p: Int => Boolean): Set = intersect(a, p)
  
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def inforall(el: Int): Boolean = {
      if (el > limit) true
      else if (contains(s, el) && p(el)) inforall(el + 1)
      else false 
    }
    inforall(-limit)
  }
  
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))
  
  def map(a: Set, f: Int => Int): Set = x => (exists(a, y => f(y) == x))
  
}