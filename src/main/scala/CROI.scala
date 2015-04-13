import Utils._

case class CROI(
                 n: List[String],
                 r: List[String],
                 c: List[String],
                 type1: Map[String, String],
                 plays: List[(String, String, String)],
                 links: Map[(String, String), List[(String, String)]]
                 ) {

  assert(mutualDisjoint(List(n, r, c, List(""))))
  assert(totalFunction(n.union(r).union(c), type1.map { case (k, v) => (k, List(v)) }))

  def compliant(crom: CROM): Boolean = crom.wellformed &&
    axiom6(crom) && axiom7(crom) && axiom8(crom) &&
    axiom9(crom) && axiom10(crom) && axiom11(crom)

  def axiom6(crom: CROM): Boolean =
    all(plays.map { case (o, c1, r1) => crom.fills.contains((type1(o), type1(r1))) && crom.parts(type1(c1)).contains(type1(r1)) })

  def axiom7(crom: CROM): Boolean =
    all(for ((o, c, r) <- plays; (o1, c1, r1) <- plays if o1 == o && c1 == c && r1 != r) yield type1(r1) != type1(r))

  def axiom8(crom: CROM): Boolean =
    all((for (r1 <- r) yield for ((o, c, r2) <- plays if r2 == r1) yield (o, c)).map(_.size == 1))

  def axiom9(crom: CROM): Boolean =
    all(for (c1 <- c; r1 <- crom.rst if links.contains((r1, c1))) yield !links((r1, c1)).contains(("", "")))

  def axiom10(crom: CROM): Boolean =
    all(for (rst1 <- crom.rst; c1 <- c if links.contains((rst1, c1)); r1 <- r; o1 <- o) yield
    any(for (r_1 <- repsilon) yield
    ((plays.contains(o1, c1, r1) && (type1(r1) == crom.rel(rst1).head)) == links((rst1, c1)).contains((r1, r_1))) && ((plays.contains(o1, c1, r1) && (type1(r1) == crom.rel(rst1).tail.head)) == links((rst1, c1)).contains((r_1, r1)))
    )
    )

  def axiom11(crom: CROM): Boolean =
    all(for (rst1 <- crom.rst; c1 <- c if links.contains((rst1, c1)); (r_1, r_2) <- links((rst1, c1)) if r_1 != "" && r_2 != "") yield
    !links(rst1, c1).contains((r_1, "")) && !links((rst1, c1)).contains(("", r_2))
    )

  def o: List[String] = n.union(c)

  def o_c(c: String): List[String] = plays.filter(_._2 == c).map(_._1)

  def repsilon: List[String] = r.union(List(""))

  def pred(rst: String, c: String, r: String): List[String] = links.contains((rst, c)) match {
    case true => links((rst, c)).filter(_._2 == r).map(_._1)
    case false => List.empty
  }

  def succ(rst: String, c: String, r: String): List[String] = links.contains((rst, c)) match {
    case true => links((rst, c)).filter(_._1 == r).map(_._2)
    case false => List.empty
  }

  def player(r: String): String = r match {
    case s: String if s.isEmpty => ""
    case _ => plays.find(_._3 == r) match {
      case Some(p) => p._1
      case _ => throw new RuntimeException(s"The given role '$r' is not played in the CROI!")
    }
  }

  def overline_links(rst: String, c: String): List[(String, String)] = links((rst, c)).map { case (r_1, r_2) => (player(r_1), player(r_2)) }
}
