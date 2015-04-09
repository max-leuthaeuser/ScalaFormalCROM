case class ConstraintModel[T](rolec: Map[T, List[((Int, Int), Any)]],
                              card: Map[T, ((Int, Int), (Int, Int))],
                              intra: (T, T => Boolean)) {

  def compliant(crom: CROM[T]): Boolean = crom.wellformed && axiom12(crom)

  def axiom12(crom: CROM[T]): Boolean = ???

  def validity(crom: CROM[T], croi: CROI[T]): Boolean = compliant(crom) && croi.compliant(crom) &&
    axiom13(crom, croi) && axiom14(crom, croi) && axiom15(crom, croi) && axiom16(crom, croi)

  def axiom13(crom: CROM[T], croi: CROI[T]): Boolean = ???

  def axiom14(crom: CROM[T], croi: CROI[T]): Boolean = ???

  def axiom15(crom: CROM[T], croi: CROI[T]): Boolean = ???

  def axiom16(crom: CROM[T], croi: CROI[T]): Boolean = ???
}
