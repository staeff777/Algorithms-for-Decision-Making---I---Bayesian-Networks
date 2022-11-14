package b_bayesian_network

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExactInferenceTest extends AnyFlatSpec with Matchers {

  val X = Variable("X", 2)
  val Y = Variable("Y", 2)
  val Z = Variable("Z", 2)
  val A = Variable("A", 2)

  val f1 = Factor(
    Assignment(X -> 0, Y -> 0) -> 0.3,
    Assignment(X -> 0, Y -> 1) -> 0.4,
    Assignment(X -> 1, Y -> 0) -> 0.2,
    Assignment(X -> 1, Y -> 1) -> 0.1
  )

  val f2 = Factor(
    Assignment(Y -> 0, Z -> 0) -> 0.2,
    Assignment(Y -> 0, Z -> 1) -> 0.0,
    Assignment(Y -> 1, Z -> 0) -> 0.3,
    Assignment(Y -> 1, Z -> 1) -> 0.5
  )

  val f3 = Factor(
    Assignment(Z -> 0, A -> 0) -> 0.2,
    Assignment(Z -> 0, A -> 1) -> 0.0,
    Assignment(Z -> 1, A -> 0) -> 0.3,
    Assignment(Z -> 1, A -> 1) -> 0.5
  )


  "Factor multiplaction" should "combine two factors with 2 variables" in {
    val f = f1 * f2

    f.table(Assignment(X -> 0, Y -> 0, Z -> 0)) shouldBe 0.06 +- 0.00001
    f.table(Assignment(X -> 0, Y -> 0, Z -> 1)) shouldBe 0.00
    f.table(Assignment(X -> 0, Y -> 1, Z -> 0)) shouldBe 0.12 +- 0.00001
    f.table(Assignment(X -> 0, Y -> 1, Z -> 1)) shouldBe 0.20 +- 0.00001
    f.table(Assignment(X -> 1, Y -> 0, Z -> 0)) shouldBe 0.04 +- 0.00001
    f.table(Assignment(X -> 1, Y -> 0, Z -> 1)) shouldBe 0.00
    f.table(Assignment(X -> 1, Y -> 1, Z -> 0)) shouldBe 0.03 +- 0.00001
    f.table(Assignment(X -> 1, Y -> 1, Z -> 1)) shouldBe 0.05 +- 0.00001
  }

  it should "combine a three var factor with a two var factor" in {
    val f = (f1 * f2) * f3
    f.variables.size shouldBe 4
    f.table.size shouldBe 16
  }

  it should "combine a two var factor with a three var factor" in {
    val f = f1 * (f2 * f3)
    f.variables.size shouldBe 4
    f.table.size shouldBe 16
  }


  "condition" should "work with a single evidence" in {
    val f = f1 * f2
    val cf = ExactInference.condition(f, X, 1)
    cf.variables.size shouldBe 2
    cf.table.size shouldBe 4
    cf.table(Assignment( Y -> 0, Z -> 0)) shouldBe 0.04 +- 0.00001
    cf.table(Assignment( Y -> 0, Z -> 1)) shouldBe 0.00
    cf.table(Assignment( Y -> 1, Z -> 0)) shouldBe 0.03 +- 0.00001
    cf.table(Assignment( Y -> 1, Z -> 1)) shouldBe 0.05 +- 0.00001
  }

  it should "work with multiple evidence" in {
    val f = f1 * f2
    val cf = ExactInference.condition(f, List((X, 1),(Y,0)))
    cf.variables.size shouldBe 1
    cf.table.size shouldBe 2
    cf.table(Assignment( Z -> 0)) shouldBe 0.04 +- 0.00001
    cf.table(Assignment( Z -> 1)) shouldBe 0.00
  }


}
