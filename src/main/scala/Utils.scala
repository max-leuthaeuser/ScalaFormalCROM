object Utils {

  def mutualDisjoint[T](sets: List[List[T]]): Boolean = {
    val all = sets.flatten
    all.size == all.toSet.size
  }

  def totalFunction[T](domain: List[T], foo: Map[T, List[T]]): Boolean = domain.toSet.subsetOf(foo.keySet)

  def all(on: List[Boolean]): Boolean = !on.contains(false)

  def any(on: List[Boolean]): Boolean = on.contains(true)

  def atoms(a: Any): List[String] = a match {
    case elem: String => List(elem)
    case elem: RoleGroup => elem.rolegroups.map(atoms).flatten
  }

  def evaluate(a: Any, croi: CROI, o: String, c: String): Int = a match {
    case elem: String => any(croi.r.toList.filter(croi.type1(_) == a).map(rr => croi.plays.contains((o, c, rr)))) match {
      case true => 1
      case false => 0
    }
    case elem: RoleGroup =>
      val sum = elem.rolegroups.map(evaluate(_, croi, o, c)).sum
      if (elem.lower <= sum && sum <= elem.upper) 1 else 0
  }
}
