object Utils {

  def mutualDisjoint[T](sets: List[Set[T]]): Boolean = {
    val all = sets.flatten
    all.size == all.toSet.size
  }

  def totalFunction[T](domain: Set[T], foo: Map[T, Set[T]]): Boolean = domain.subsetOf(foo.keySet)

  def all(on: Set[Boolean]): Boolean = !on.contains(false)

  def any(on: Set[Boolean]): Boolean = on.contains(true)

  def atoms(a: Any): Set[String] = a match {
    case elem: String => Set(elem)
    case elem: RoleGroup => elem.rolegroups.map(atoms).flatten
  }

  def evaluate(a: Any, croi: CROI, o: String, c: String): Int = a match {
    case elem: String => any(croi.r.filter(croi.type1(_) == a).map(rr => croi.plays.contains((o, c, rr)))) match {
      case true => 1
      case false => 0
    }
    case elem: RoleGroup =>
      val sum = elem.rolegroups.map(evaluate(_, croi, o, c)).sum
      if (elem.lower <= sum && sum <= elem.upper) 1 else 0
  }
}
