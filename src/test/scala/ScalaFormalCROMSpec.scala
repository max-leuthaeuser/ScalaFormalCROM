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
}
