import org.scalatest.{Matchers, GivenWhenThen, FeatureSpec}
import Utils._

class ScalaFormalCROMSpec extends FeatureSpec with GivenWhenThen with Matchers {
  info("Test spec for ScalaFormalCROM.")

  feature("Testing util functions") {
    scenario("Testing function for mutually disjoint sets") {
      mutualDisjoint[Int](List(Set.empty, Set.empty)) shouldBe true
      mutualDisjoint(List(Set(1, 2), Set(3, 4))) shouldBe true
      mutualDisjoint[Int](List(Set(1), Set(1))) shouldBe false
      mutualDisjoint[Int](List(Set.empty, Set(1), Set(1))) shouldBe false
      mutualDisjoint(List(Set(1, 2, 3), Set(3, 4, 5))) shouldBe false
    }

    scenario("Testing function for total function") {
      totalFunction(Set.empty, Map(1 -> Set(1))) shouldBe true
      totalFunction(Set(1, 2), Map(1 -> Set(1), 2 -> Set(2))) shouldBe true
      totalFunction(Set(1, 2), Map(1 -> Set(1))) shouldBe false
      totalFunction[Int](Set(1), Map.empty) shouldBe false
    }
  }

  feature("Testing CROM") {
    val test0 = CROM(Set.empty, Set.empty, Set.empty, Set.empty, Set.empty, Map.empty, Map.empty)
    val test1 = CROM(Set("1"), Set("2", "3"), Set("4"), Set("a"), Set(("1", "2"), ("1", "3")), Map("4" -> Set("2", "3")), Map("a" -> Set("2", "3")))
    val test2 = CROM(Set("1"), Set("2", "3"), Set("4"), Set("a"), Set(("1", "2")), Map("4" -> Set("2", "3")), Map("a" -> Set("2", "3")))
    val test3 = CROM(Set("1"), Set("2", "3"), Set("4", "5"), Set("a"), Set(("1", "2"), ("1", "3")), Map("4" -> Set("2", "3"), "5" -> Set.empty), Map("a" -> Set("2", "3")))
    val test4 = CROM(Set("1"), Set("2", "3"), Set("4", "5"), Set("a"), Set(("1", "2"), ("1", "3")), Map("4" -> Set("2", "3"), "5" -> Set("2")), Map("a" -> Set("2", "3")))
    val test5 = CROM(Set("1"), Set("2", "3"), Set("4"), Set("a"), Set(("1", "2"), ("1", "3")), Map("4" -> Set("2", "3")), Map("a" -> Set("2", "2")))
    val test6 = CROM(Set("1"), Set("2", "3"), Set("4", "5"), Set("a"), Set(("1", "2"), ("1", "3")), Map("4" -> Set("2"), "5" -> Set("3")), Map("a" -> Set("2", "3")))
    val test7 = CROM(Set("1"), Set("2", "3", "4"), Set("5", "6"), Set("a"), Set(("1", "2"), ("1", "3")), Map("5" -> Set("3"), "6" -> Set.empty), Map("a" -> Set("2", "2")))

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

  }

  feature("Testing Role Groups") {

  }

  feature("Testing Constraint Models") {

  }

  feature("Testing Validity") {

  }
}
