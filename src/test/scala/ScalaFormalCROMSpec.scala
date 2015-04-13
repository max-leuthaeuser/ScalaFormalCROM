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
    val test10 = CROI(List("1"), List("2", "3"), List("4"), Map("1" -> "1", "2" -> "2", "3" -> "2", "4" -> "4"), List(("1", "4", "2"), ("1", "4", "3")), Map(("a", "4") -> List(("2", ""),("3",""))))
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

  }

  feature("Testing Constraint Models") {

  }

  feature("Testing Validity") {

  }
}
