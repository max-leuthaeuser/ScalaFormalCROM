case class ConstraintModel(rolec: Map[String, List[((Int, Int), Any)]],
                              card: Map[String, ((Int, Int), (Int, Int))],
                              intra: (String, String => Boolean)) {

  def compliant(crom: CROM): Boolean = crom.wellformed && axiom12(crom)

  def axiom12(crom: CROM): Boolean = ???

  def validity(crom: CROM, croi: CROI): Boolean = compliant(crom) && croi.compliant(crom) &&
    axiom13(crom, croi) && axiom14(crom, croi) && axiom15(crom, croi) && axiom16(crom, croi)

  def axiom13(crom: CROM, croi: CROI): Boolean = ???

  def axiom14(crom: CROM, croi: CROI): Boolean = ???

  def axiom15(crom: CROM, croi: CROI): Boolean = ???

  def axiom16(crom: CROM, croi: CROI): Boolean = ???
}
