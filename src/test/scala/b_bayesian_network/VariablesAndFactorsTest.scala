package b_bayesian_network
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class VariablesAndFactorsTest extends AnyFlatSpec with Matchers {

  val variable1 = Variable("var1", 2)
  val variable2 = Variable("var2", 2)

  "a Variable" should "describe a discrete distribution with a name" in {
    variable1.name shouldBe "var1"
  }

  it should "give the amount of its' possible values in the property states" in {
    variable1.states shouldBe 2
  }

  "an Assignment" should "maps one ore more variable to each one's state (one of the n states)" in {
    val assignment1 = Assignment(variable1 -> 1)
    val assignment2 = Assignment(variable1 -> 2, variable2 -> 1)
    assignment1 shouldNot equal(assignment2)
  }

  it should "be equal to another Assignment of the same variable, with the same value" in {
    val assignment1 = Assignment(variable1 -> 1)
    val assignment2 = Assignment(variable2 -> 1)
    assignment1 should equal(assignment2)
  }

  it should "not consider the order of the variables for equality" in {
    val assignment1 = Assignment(variable1 -> 2, variable2 -> 1)
    val assignment2 = Assignment(variable2 -> 1, variable1 -> 2)
    assignment1 should equal(assignment2)
  }

  "a Factor" should "combine assignments with a probability value and thus build a factor table" in {
    val factor = Factor(
      Assignment(variable1 -> 2, variable2 -> 1) -> 0.9,
      Assignment(variable1 -> 1, variable2 -> 1) -> 0.1,
      Assignment(variable1 -> 1, variable2 -> 2) -> 0.05,
      Assignment(variable1 -> 2, variable2 -> 2) -> 0.3
    )

    factor.table.size shouldBe 4
  }

  it should "contain a normalize function, that normalizes to probabilities to sum up to 1" in {
    val factor = Factor(
      Assignment(variable1 -> 2, variable2 -> 1) -> 0.9,
      Assignment(variable1 -> 1, variable2 -> 1) -> 0.1,
      Assignment(variable1 -> 1, variable2 -> 2) -> 0.05,
      Assignment(variable1 -> 2, variable2 -> 2) -> 0.3
    )
    val factor2 = factor.normalize
    factor2.table.values.sum shouldBe 1.0 +- 0.00001
  }

  it should "contain a selection function, that takes an assignment and returns all " +
    "Variable->State pairs *whiches* Variable also exists in the Factor Variables" in {
      val factor = Factor(
        Assignment(variable1 -> 2, variable2 -> 1) -> 0.9,
        Assignment(variable1 -> 1, variable2 -> 1) -> 0.1,
        Assignment(variable1 -> 1, variable2 -> 2) -> 0.05,
        Assignment(variable1 -> 2, variable2 -> 2) -> 0.3
      )
      val variable3  = Variable("var3", 2)
      val assignment = Assignment(variable3 -> 1, variable2 -> 2)

      val result = factor.select_existing_variables_from_assignment(assignment)

      result.size shouldBe 1
      result.keys.head should equal(variable2)
      result.values.head should equal(2)

    }
}
