package b_bayesian_network

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class complete_bayesian_network_test extends AnyFlatSpec with Matchers {

  val battery       = Variable("Battery", 2)
  val solar         = Variable("Solar", 2)
  val energy        = Variable("Energy", 2)
  val deviation     = Variable("Deviation", 2)
  val communication = Variable("Communication", 2)

  val x = Assignment(battery -> 1)
  val bayesian_network = Bayesian_Network(
    Factor(Assignment(battery -> 1) -> 0.99, Assignment(battery -> 2) -> 0.01),
    Factor(Assignment(solar -> 1)   -> 0.98, Assignment(solar -> 2)   -> 0.02),
    Factor(
      Assignment(energy -> 1, battery -> 1, solar -> 1) -> 0.90,
      Assignment(energy -> 1, battery -> 1, solar -> 2) -> 0.04,
      Assignment(energy -> 1, battery -> 2, solar -> 1) -> 0.05,
      Assignment(energy -> 1, battery -> 2, solar -> 2) -> 0.01,
      Assignment(energy -> 2, battery -> 1, solar -> 1) -> 0.10,
      Assignment(energy -> 2, battery -> 1, solar -> 2) -> 0.96,
      Assignment(energy -> 2, battery -> 2, solar -> 1) -> 0.95,
      Assignment(energy -> 2, battery -> 2, solar -> 2) -> 0.99
    ),
    Factor(
      Assignment(deviation -> 1, energy -> 1) -> 0.96,
      Assignment(deviation -> 1, energy -> 2) -> 0.03,
      Assignment(deviation -> 2, energy -> 1) -> 0.04,
      Assignment(deviation -> 2, energy -> 2) -> 0.97
    ),
    Factor(
      Assignment(communication -> 1, energy -> 1) -> 0.98,
      Assignment(communication -> 1, energy -> 2) -> 0.01,
      Assignment(communication -> 2, energy -> 1) -> 0.02,
      Assignment(communication -> 2, energy -> 2) -> 0.99
    )
  )

  bayesian_network.addEdge(battery, energy)
  bayesian_network.addEdge(solar, energy)
  bayesian_network.addEdge(energy, deviation)
  bayesian_network.addEdge(energy, communication)

  "The bayesian network" should "have 5 Factors" in {
    assert(bayesian_network.factors.size == 5)
  }

  it should "return the correct probability for an assignment" in {
    val as = Assignment(battery -> 1, solar -> 1, energy -> 1, deviation -> 1, communication -> 1)
    bayesian_network.probability(as) shouldBe 0.82 +- 0.01
  }

  "The Graphtools" should "render the network graph" in {
    if (GraphTools.default_file.exists()) GraphTools.default_file.delete()
    GraphTools.renderToFile(bayesian_network.graph)
    assert(GraphTools.default_file.exists())
  }

}
