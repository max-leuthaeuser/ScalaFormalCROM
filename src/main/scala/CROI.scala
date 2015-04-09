import Utils._

case class CROI[T](
                    n: Set[T],
                    r: Set[T],
                    c: Set[T],
                    type1: Map[T, T],
                    plays: Set[(T, T, T)],
                    links: Map[(T, T), Set[(T, T)]]
                    ) {

  // TODO: check this None
  assert(mutualDisjoint(List(n, r, c /*, Set(None)*/)))
  assert(totalFunction(n.union(r).union(c), type1.map { case (k, v) => (k, Set(v)) }))

  def compliant(crom: CROM[T]): Boolean = crom.wellformed &&
    axiom6(crom) && axiom7(crom) && axiom8(crom) &&
    axiom9(crom) && axiom10(crom) && axiom11(crom)

  def axiom6(crom: CROM[T]): Boolean = ???

  def axiom7(crom: CROM[T]): Boolean = ???

  def axiom8(crom: CROM[T]): Boolean = ???

  def axiom9(crom: CROM[T]): Boolean = ???

  def axiom10(crom: CROM[T]): Boolean = ???

  def axiom11(crom: CROM[T]): Boolean = ???

  def o: Set[T] = n.union(c)

  def o_c(c: T): Set[T] = plays.filter(_._2 == c).map(_._1)

  // TODO: check this None
  def repsilon: Set[T] = r ++ None

  def pred(rst: T, c: T, r: T): Set[T] = links.contains((rst, c)) match {
    case true => links((rst, c)).filter(_._2 == r).map(_._1)
    case false => Set.empty
  }

  def succ(rst: T, c: T, r: T): Set[T] = links.contains((rst, c)) match {
    case true => links((rst, c)).filter(_._1 == r).map(_._2)
    case false => Set.empty
  }

  // TODO: check this None
  def player(r: T): Any = r match {
    /* case None => None */
    case _ => plays.find(_._3 == r) match {
      case Some(p) => p._1
      case _ => throw new RuntimeException(s"The given role '$r' is not played in the CROI!")
    }
  }

  def overline_links(rst: T, c: T): Set[(T, T)] = links((rst, c)).map { case (r_1, r_2) => (player(r_1).asInstanceOf[T], player(r_2).asInstanceOf[T]) }
}
