case class RoleGroup(rolegroups: Set[Any], lower: Int, upper: Int) {

  assert(0 <= lower && lower <= upper)

}
