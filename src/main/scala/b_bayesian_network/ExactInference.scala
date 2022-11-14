package b_bayesian_network

object ExactInference {

  def condition(f: Factor, variable: Variable, value: Double): Factor = if (f.variables.contains(variable)) {
    val new_table = f.table.collect {
      case (a, p) if (a(variable) == value) =>
        a.filter(_._1 != variable) -> p
    }
    Factor(new_table)
  } else f

  def condition(f: Factor, evidence: List[(Variable, Double)]): Factor = {
    evidence.foldLeft(f) { case (f, e) => condition(f, e._1, e._2) }
  }

}
