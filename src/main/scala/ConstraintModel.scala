import Utils._

case class ConstraintModel(rolec: Map[String, List[((Int, Int), Any)]],
                           card: Map[String, ((Int, Int), (Int, Int))],
                           intra: Set[(String, (Any) => Boolean)]) {

  def compliant(crom: CROM): Boolean = crom.wellformed && axiom12(crom)

  def axiom12(crom: CROM): Boolean =
    all(for (ct1 <- crom.ct if rolec.contains(ct1); (crd, a) <- rolec(ct1)) yield
    atoms(a).subsetOf(crom.parts(ct1))
    )

  def validity(crom: CROM, croi: CROI): Boolean = compliant(crom) && croi.compliant(crom) &&
    axiom13(crom, croi) && axiom14(crom, croi) && axiom15(crom, croi) && axiom16(crom, croi)

  def axiom13(crom: CROM, croi: CROI): Boolean =
    all(for (ct1 <- crom.ct if rolec.contains(ct1); (crd, a) <- rolec(ct1); c1 <- croi.c if croi.type1(c1) == ct1) yield {
      val sum = croi.o_c(c1).map(evaluate(a, croi, _, c1)).sum
      crd._1 <= sum && sum <= crd._2
    }
    )

  def axiom14(crom: CROM, croi: CROI): Boolean =
    all(for ((o, c, r) <- croi.plays if rolec.contains(croi.type1(c)); (crd, a) <- rolec(croi.type1(c)) if atoms(a).contains(croi.type1(r))) yield
    evaluate(a, croi, o, c) == 1
    )

  def axiom15(crom: CROM, croi: CROI): Boolean =
    all(for (rst <- crom.rst if card.contains(rst); c <- croi.c if croi.links.contains(rst, c); (r_1, r_2) <- croi.links((rst, c))) yield {
      val l1 = croi.pred(rst, c, r_2).size
      val l2 = croi.succ(rst, c, r_1).size
      card(rst)._1._1 <= l1 && l1 <= card(rst)._1._2 && card(rst)._2._1 <= l2 && l2 <= card(rst)._2._2
    }
    )

  def axiom16(crom: CROM, croi: CROI): Boolean =
    all(for (c <- croi.c; (rst, f) <- intra if croi.links.contains((rst, c))) yield f(croi.overline_links(rst, c)))
}
