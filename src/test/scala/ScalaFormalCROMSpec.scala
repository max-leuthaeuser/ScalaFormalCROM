import org.scalatest.{Matchers, GivenWhenThen, FeatureSpec}
import Utils._

class ScalaFormalCROMSpec extends FeatureSpec with GivenWhenThen with Matchers {
  info("Test spec for ScalaFormalCROM.")

  feature("Testing util functions") {
    scenario("Testing function for mutually disjoint Lists") {
      mutualDisjoint[Int](List(List.empty, List.empty)) shouldBe true
      mutualDisjoint(List(List(1, 2), List(3, 4))) shouldBe true
      mutualDisjoint[Int](List(List(1), List(1))) shouldBe false
      mutualDisjoint[Int](List(List.empty, List(1), List(1))) shouldBe false
      mutualDisjoint(List(List(1, 2, 3), List(3, 4, 5))) shouldBe false
    }

    scenario("Testing function for total function") {
      totalFunction(List.empty, Map(1 -> List(1))) shouldBe true
      totalFunction(List(1, 2), Map(1 -> List(1), 2 -> List(2))) shouldBe true
      totalFunction(List(1, 2), Map(1 -> List(1))) shouldBe false
      totalFunction[Int](List(1), Map.empty) shouldBe false
    }
  }

  feature("Testing CROM") {
    val test0 = CROM(List.empty, List.empty, List.empty, List.empty, List.empty, Map.empty, Map.empty)
    val test1 = CROM(List("1"), List("2", "3"), List("4"), List("a"), List(("1", "2"), ("1", "3")), Map("4" -> List("2", "3")), Map("a" -> List("2", "3")))
    val test2 = CROM(List("1"), List("2", "3"), List("4"), List("a"), List(("1", "2")), Map("4" -> List("2", "3")), Map("a" -> List("2", "3")))
    val test3 = CROM(List("1"), List("2", "3"), List("4", "5"), List("a"), List(("1", "2"), ("1", "3")), Map("4" -> List("2", "3"), "5" -> List.empty), Map("a" -> List("2", "3")))
    val test4 = CROM(List("1"), List("2", "3"), List("4", "5"), List("a"), List(("1", "2"), ("1", "3")), Map("4" -> List("2", "3"), "5" -> List("2")), Map("a" -> List("2", "3")))
    val test5 = CROM(List("1"), List("2", "3"), List("4"), List("a"), List(("1", "2"), ("1", "3")), Map("4" -> List("2", "3")), Map("a" -> List("2", "2")))
    val test6 = CROM(List("1"), List("2", "3"), List("4", "5"), List("a"), List(("1", "2"), ("1", "3")), Map("4" -> List("2"), "5" -> List("3")), Map("a" -> List("2", "3")))
    val test7 = CROM(List("1"), List("2", "3", "4"), List("5", "6"), List("a"), List(("1", "2"), ("1", "3")), Map("5" -> List("3"), "6" -> List.empty), Map("a" -> List("2", "2")))

    val cromtests = Seq((test0, true, true, true, true, true),
      (test1, true, true, true, true, true), (test2, false, true, true, true, true),
      (test3, true, false, true, true, true), (test4, true, true, false, true, true),
      (test5, true, true, true, false, true), (test6, true, true, true, true, false),
      (test7, false, false, false, false, false))

    for ((t, a1, a2, a3, a4, a5) <- cromtests) {
      t.axiom1 shouldBe a1
      t.axiom2 shouldBe a2
      t.axiom3 shouldBe a3
      t.axiom4 shouldBe a4
      t.axiom5 shouldBe a5
      t.wellformed shouldBe (a1 && a2 && a3 && a4 && a5)
    }
  }

  feature("Testing CROI") {
    val test1 = CROM(List("1"), List("2", "3"), List("4"), List("a"), List(("1", "2"), ("1", "3")), Map("4" -> List("2", "3")), Map("a" -> List("2", "3")))
    val test8 = CROI(List("1"), List("2", "3"), List("4"), Map("1" -> "1", "2" -> "2", "3" -> "3", "4" -> "4"), List(("1", "4", "2"), ("1", "4", "3")), Map(("a", "4") -> List(("2", "3"))))
    val test8b = CROI(List.empty, List.empty, List.empty, Map.empty, List.empty, Map.empty)
    val test9 = CROI(List("1"), List("2", "3"), List("4"), Map("1" -> "1", "2" -> "5", "3" -> "3", "4" -> "4"), List(("1", "4", "2"), ("1", "4", "3")), Map(("a", "4") -> List(("2", "3"))))
    val test10 = CROI(List("1"), List("2", "3"), List("4"), Map("1" -> "1", "2" -> "2", "3" -> "2", "4" -> "4"), List(("1", "4", "2"), ("1", "4", "3")), Map(("a", "4") -> List(("2", ""), ("3", ""))))
    val test11 = CROI(List("1"), List("2", "3"), List("4", "5"), Map("1" -> "1", "2" -> "2", "3" -> "3", "4" -> "4", "5" -> "4"), List(("1", "4", "2"), ("1", "5", "2"), ("1", "4", "3")), Map(("a", "4") -> List(("2", "3"))))
    val test11b = CROI(List("1", "5"), List("2", "3"), List("4"), Map("1" -> "1", "2" -> "2", "3" -> "3", "4" -> "4", "5" -> "1"), List(("1", "4", "2"), ("5", "4", "2"), ("1", "4", "3")), Map(("a", "4") -> List(("2", "3"))))
    val test12 = CROI(List("1"), List("2", "3"), List("4"), Map("1" -> "1", "2" -> "2", "3" -> "3", "4" -> "4"), List(("1", "4", "2"), ("1", "4", "3")), Map(("a", "4") -> List(("2", "3"), ("", ""))))
    val test13 = CROI(List("1", "6"), List("2", "3", "5"), List("4"), Map("1" -> "1", "2" -> "2", "3" -> "3", "4" -> "4", "5" -> "3", "6" -> "1"), List(("1", "4", "2"), ("1", "4", "3"), ("6", "4", "5")), Map(("a", "4") -> List(("2", "3"))))
    val test13b = CROI(List("1", "6"), List("2", "3", "5"), List("4"), Map("1" -> "1", "2" -> "2", "3" -> "3", "5" -> "2", "6" -> "1", "4" -> "4"), List(("1", "4", "2"), ("1", "4", "3"), ("6", "4", "5")), Map(("a", "4") -> List(("2", "3"))))
    val test14 = CROI(List("1"), List("2", "3"), List("4"), Map("1" -> "1", "2" -> "2", "3" -> "3", "4" -> "4"), List(("1", "4", "2"), ("1", "4", "3")), Map(("a", "4") -> List(("2", "3"), ("2", ""))))
    val test14b = CROI(List("1"), List("2", "3"), List("4"), Map("1" -> "1", "2" -> "2", "3" -> "3", "4" -> "4"), List(("1", "4", "2"), ("1", "4", "3")), Map(("a", "4") -> List(("2", "3"), ("", "3"))))
    val test15 = CROI(List("1"), List("2", "3", "5", "6"), List("4"), Map("1" -> "1", "2" -> "5", "3" -> "3", "4" -> "4", "5" -> "3", "6" -> "2"), List(("1", "4", "2"), ("1", "4", "5"), ("1", "4", "3")), Map(("a", "4") -> List(("2", "3"), ("", ""), ("2", ""))))

    val croitests = Seq((test8, true, true, true, true, true, true),
      (test8b, true, true, true, true, true, true),
      (test9, false, true, true, true, true, true),
      (test10, true, false, true, true, true, true),
      (test11, true, true, false, true, true, true),
      (test11b, true, true, false, true, true, true),
      (test12, true, true, true, false, true, true),
      (test13, true, true, true, true, false, true),
      (test13b, true, true, true, true, false, true),
      (test14, true, true, true, true, true, false),
      (test14b, true, true, true, true, true, false),
      (test15, false, false, false, false, false, false))

    for ((t, a6, a7, a8, a9, a10, a11) <- croitests) {
      t.axiom6(test1) shouldBe a6
      t.axiom7(test1) shouldBe a7
      t.axiom8(test1) shouldBe a8
      t.axiom9(test1) shouldBe a9
      t.axiom10(test1) shouldBe a10
      t.axiom11(test1) shouldBe a11
      t.compliant(test1) shouldBe (a6 && a7 && a8 && a9 && a10 && a11)
    }
  }

  feature("Testing Role Groups") {
    val testrg1 = "2"
    val testrg2 = RoleGroup(List("2", "3"), 2, 2)
    val testrg3 = RoleGroup(List(RoleGroup(List("2", RoleGroup(List("3"), 1, 2)), 0, 1), "2"), 1, 1)
    val testrg4 = RoleGroup(List.empty, 0, 0)
    val testrg5 = RoleGroup(List.empty, 1, 1)
    val testrg6 = RoleGroup(List("2"), 0, 0)
    val testrg7 = RoleGroup(List("5"), 0, 0)

    val test8 = CROI(List("1"), List("2", "3"), List("4"), Map("1" -> "1", "2" -> "2", "3" -> "3", "4" -> "4"), List(("1", "4", "2"), ("1", "4", "3")), Map(("a", "4") -> List(("2", "3"))))

    val rgtests = Seq((testrg1, List("2"), 1), (testrg2, List("2", "3"), 1),
      (testrg3, List("2", "3"), 1), (testrg4, List.empty, 1),
      (testrg5, List.empty, 0), (testrg6, List("2"), 0),
      (testrg7, List("5"), 1))

    for ((t, s, e) <- rgtests) {
      atoms(t) shouldBe s
      evaluate(t, test8, "1", "4") shouldBe e
    }
  }

  feature("Testing Constraint Models") {
    val test0 = CROM(List.empty, List.empty, List.empty, List.empty, List.empty, Map.empty, Map.empty)
    val test1 = CROM(List("1"), List("2", "3"), List("4"), List("a"), List(("1", "2"), ("1", "3")), Map("4" -> List("2", "3")), Map("a" -> List("2", "3")))

    val order = (r: List[(String, String)]) => Utils.all(for ((x, y) <- r) yield x <= y)

    val rgxor = RoleGroup(List("2", "3"), 1, 1)
    val testcm0 = ConstraintModel(Map.empty, Map.empty, List.empty)

    val testcm1 = ConstraintModel(Map("4" -> List(((1, 3), rgxor))), Map("a" ->((1, 1), (1, 1))), List(("a", order)))
    val testcm2 = ConstraintModel(Map("4" -> List(((1, 1), "2"))), Map("a" ->((1, 1), (1, 1))), List.empty)
    val testcm3 = ConstraintModel(Map("4" -> List(((1, 1), "2"))), Map.empty, List.empty)
    val testcm4 = ConstraintModel(Map("4" -> List(((1, 1), "5"))), Map.empty, List.empty)
    val testcm5 = ConstraintModel(Map("5" -> List(((1, 1), "2"))), Map.empty, List.empty)

    val cmtests = Seq((testcm0, true), (testcm1, true), (testcm2, true),
      (testcm3, true), (testcm4, false), (testcm5, true))

    for ((t, a12) <- cmtests) {
      t.axiom12(test1) shouldBe a12
      t.compliant(test1) shouldBe a12
    }

    testcm0.compliant(test0) shouldBe true
  }

  feature("Testing Validity") {

  }
}
