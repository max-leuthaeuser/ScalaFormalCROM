import Utils._

object CROI {
  def empty[T >: Null]: CROI[T] = CROI[T](List.empty, List.empty, List.empty, Map.empty, List.empty, Map.empty)
}

case class CROI[T >: Null](
                            n: List[T],
                            r: List[T],
                            c: List[T],
                            type1: Map[T, T],
                            plays: List[(T, T, T)],
                            links: Map[(T, T), List[(T, T)]]
                            ) {

  assert(mutualDisjoint(List(n, r, c, List(null))))
  assert(totalFunction(n.union(r).union(c), type1.map { case (k, v) => (k, List(v)) }))

  def compliant(crom: CROM[T]): Boolean = crom.wellformed &&
    axiom6(crom) && axiom7(crom) && axiom8(crom) &&
    axiom9(crom) && axiom10(crom) && axiom11(crom)

  def axiom6(crom: CROM[T]): Boolean =
    all(plays.map { case (o, c1, r1) => crom.fills.contains((type1(o), type1(r1))) && crom.parts(type1(c1)).contains(type1(r1)) })

  def axiom7(crom: CROM[T]): Boolean =
    all(for ((o, c, r) <- plays; (o1, c1, r1) <- plays if o1 == o && c1 == c && r1 != r) yield type1(r1) != type1(r))

  def axiom8(crom: CROM[T]): Boolean =
    all((for (r1 <- r) yield for ((o, c, r2) <- plays if r2 == r1) yield (o, c)).map(_.size == 1))

  def axiom9(crom: CROM[T]): Boolean =
    all(for (c1 <- c; r1 <- crom.rst if links.contains((r1, c1))) yield !links((r1, c1)).contains((null, null)))

  def axiom10(crom: CROM[T]): Boolean =
    all(for (rst1 <- crom.rst; c1 <- c if links.contains((rst1, c1)); r1 <- r; o1 <- o) yield
    any(for (r_1 <- repsilon) yield
    ((plays.contains(o1, c1, r1) && (type1(r1) == crom.rel(rst1).head)) == links((rst1, c1)).contains((r1, r_1))) && ((plays.contains(o1, c1, r1) && (type1(r1) == crom.rel(rst1).tail.head)) == links((rst1, c1)).contains((r_1, r1)))
    )
    )

  def axiom11(crom: CROM[T]): Boolean =
    all(for (rst1 <- crom.rst; c1 <- c if links.contains((rst1, c1)); (r_1, r_2) <- links((rst1, c1)) if r_1 != null && r_2 != null) yield
    !links(rst1, c1).contains((r_1, null)) && !links((rst1, c1)).contains((null, r_2))
    )

  def o: List[T] = n.union(c)

  def o_c(c: T): List[T] = plays.filter(_._2 == c).map(_._1)

  def repsilon: List[T] = r :+ null

  def pred(rst: T, c: T, r: T): List[T] = links.contains((rst, c)) match {
    case true => links((rst, c)).filter(_._2 == r).map(_._1)
    case false => List.empty
  }

  def succ(rst: T, c: T, r: T): List[T] = links.contains((rst, c)) match {
    case true => links((rst, c)).filter(_._1 == r).map(_._2)
    case false => List.empty
  }

  def player(r: T): T = r match {
    case null => null
    case _ => plays.find(_._3 == r) match {
      case Some(p) => p._1
      case _ => throw new RuntimeException(s"The given role '$r' is not played in the CROI!")
    }
  }

  def overline_links(rst: T, c: T): List[(T, T)] = links((rst, c)).map { case (r_1, r_2) => (player(r_1), player(r_2)) }
}
