package b_bayesian_network

import de.vandermeer.asciitable.AsciiTable
import guru.nidi.graphviz.engine.{Format, Graphviz}
import guru.nidi.graphviz.parse.Parser
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import org.jgrapht.nio.dot.DOTExporter
import org.jgrapht.nio.{Attribute, DefaultAttribute}

import java.io.File
import java.util.Locale
import scala.collection.mutable
import scala.util.Success
import scala.jdk.CollectionConverters.*
import java.text.NumberFormat

case class Variable(name: String, states: Int) {
  override def toString = name
}

type Assignment = Map[Variable, Int]

object Assignment {
  def apply(entries: (Variable, Int)*): Assignment = entries.toMap
}

type FactorTable = Map[Assignment, Double]

object FactorTable {
  def apply(entries: (Assignment, Double)*): FactorTable = entries.toMap
}

object Factor {
  def apply(entries: (Assignment, Double)*): Factor = Factor(entries.toMap)
}

case class Factor(table: FactorTable) {

  val variables = table.keys.map(_.keys).flatten.toSet

  def normalize = {
    val sum       = table.values.sum
    val new_table = table.map { case (a, v) => a -> v / sum }
    Factor(new_table)
  }

  def select_existing_variables_from_assignment(assignment: Assignment) =
    assignment.filter((variable, _) => variables.contains(variable)).toMap

  /** An implementation * of the factor product, which constructs the factor representing the joint distribution of two
    * smaller factors
    *
    * @param factor
    */
  def *(f2: Factor) = {
    val f1                  = this
    val unique_f2_variables = f2.variables.diff(f1.variables) // contains all variables that are not in f1 (e.g. X)
    val unique_f2_assignments = f2.table
      .map(_._1.filter(assignment => unique_f2_variables.contains(assignment._1)))
      .toSet // contains all assignments e.g. X->0, X->1

    val new_table = for ((f1_assignment, f1_p) <- f1.table; unique_f2_assignment <- unique_f2_assignments) yield {
      val new_assignment = f1_assignment ++ unique_f2_assignment
      val f2_assignment  = new_assignment.filter { case (k, _) => f2.variables.contains(k) }
      val f2_p           = f2.table.getOrElse(f2_assignment, 0.0)

      new_assignment -> f1_p * f2_p
    }

    Factor(new_table.toMap)
  }

  override def toString: String = {
    val at     = new AsciiTable()
    val header = variables.map(_.name).toArray :+ "P"
    at.addRule()
    at.addRow(header.toArray: _*)
    at.addRule()

    val nf = NumberFormat.getInstance(Locale.US)
    nf.setMaximumFractionDigits(6)
    def get(l: Vector[Int], i: Int) = l.lift(i).getOrElse(0)
    def list_to_tuple(l: Vector[Int]) = // create a tuple list of variable values, for sorting
      (get(l, 0), get(l, 1), get(l, 2), get(l, 3), get(l, 4), get(l, 5), get(l, 6), get(l, 7), get(l, 8))
    for ((a, p) <- table.toArray.sortBy { x => list_to_tuple(x._1.toVector.map(_._2)) }) {
      val row = a.map(_._2.toString).toArray :+ nf.format(p)
      at.addRow(row.toArray: _*)
    }

    at.addRule()
    at.render()
  }
}

object BayesianNetwork {

  def apply(factors: Factor*) = new BayesianNetwork(factors.toVector)

}

case class BayesianNetwork(
    factors: Vector[Factor],
    graph: DefaultDirectedGraph[Variable, DefaultEdge] =
      new DefaultDirectedGraph[Variable, DefaultEdge](classOf[DefaultEdge])
) {

  val variables = factors.map(_.variables).flatten.toSet
  variables.foreach(v => graph.addVertex(v))

  def addEdge(from: Variable, to: Variable) = graph.addEdge(from, to)

  def generateGraphFromFactors(): Unit = {
    val variable_combinations                     = factors.toVector.map(_.variables)
    val (starting_variables, remaining_variables) = variable_combinations.partition(_.size == 1)

    /** recursive function to generate the graph
      * @param current_root_nodes
      * @param remaining_nodes
      */
    def createGraph(current_root_nodes: Set[Variable], remaining_nodes: Set[Set[Variable]]): Unit = {

      val possible_root_combinations =
        current_root_nodes
          .subsets()
          .filter(_.size > 0)
          .toVector // create a list of all possible combinations

      val new_root_nodes_opt = for (node <- remaining_nodes; possible_roots <- possible_root_combinations) yield {
        val possible_root_variables = possible_roots
        val diff                    = node.diff(possible_root_variables)
        if (diff.size == 1) {
          val target = diff.head
          possible_root_variables.foreach { case root =>
            addEdge(root, target)
          }
          Some(target, node)
        } else None
      }

      val (new_root_nodes, no_longer_remaining) = new_root_nodes_opt.flatten.unzip
      val new_remaining_nodes                   = remaining_nodes.diff(no_longer_remaining)
      if (new_root_nodes.size > 0 && new_remaining_nodes.size > 0)
        createGraph(new_root_nodes, new_remaining_nodes)
    }

    createGraph(starting_variables.flatten.toSet, remaining_variables.toSet)

  }

  def probability(assignment: Assignment) = {

    // all probabilities of one factor
    // todo has the assignment all factors?
    // was subassignment ??

    // first find variables from assignment in factor
    // then find the correct value for the assignment in factor

    val all_probabilities = factors.map(f => {
      val subassignment_with_factor_variables = f.select_existing_variables_from_assignment(assignment)
      val probabilities                       = f.table(subassignment_with_factor_variables)
      probabilities
    })

    all_probabilities.product

  }
}

object test extends App {

  val battery       = Variable("Battery", 2)
  val solar         = Variable("Solar", 2)
  val energy        = Variable("Energy", 2)
  val deviation     = Variable("Deviation", 2)
  val communication = Variable("Communication", 2)

  val bn = BayesianNetwork(
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
  /* // Old variant to create graph manuelly
  bn.addEdge(battery, energy)
  bn.addEdge(solar, energy)
  bn.addEdge(energy, deviation)
  bn.addEdge(energy, communication)
   */
  bn.generateGraphFromFactors()

  GraphTools.renderToFile(bn.graph)
  val as = Assignment(battery -> 1, solar -> 1, energy -> 1, deviation -> 1, communication -> 1)
  print(bn.probability(as))

}
