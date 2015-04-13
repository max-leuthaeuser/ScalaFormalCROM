import Utils._

case class CROM(
                    nt: Set[String],
                    rt: Set[String],
                    ct: Set[String],
                    rst: Set[String],
                    fills: Set[(String, String)],
                    parts: Map[String, Set[String]],
                    rel: Map[String, Set[String]]
                    ) {

  assert(mutualDisjoint(List(nt, rt, ct, rst)))
  assert(totalFunction(ct, parts))
  assert(totalFunction(ct, rel))

  def wellformed: Boolean = axiom1 && axiom2 && axiom3 && axiom4 && axiom5

  def axiom1: Boolean =
    all(rt.map(r => any(nt.union(ct).map(t => fills.contains((t, r))))))

  def axiom2: Boolean =
    all(ct.map(c => parts(c).size > 0))

  // TODO: check this
  def axiom3: Boolean =
    all(rt.map(r => ct.forall(!parts(_).contains(r))))

  def axiom4: Boolean =
    all(rst.map(r => rel(r).head != rel(r).tail.head))

  def axiom5: Boolean =
    all(rst.map(r => any(ct.map(c => rel(r).subsetOf(parts(c))))))
}