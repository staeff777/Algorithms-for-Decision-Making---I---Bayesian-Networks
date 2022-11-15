package b_bayesian_network

object ExactInference {

  def infer(bn: BayesianNetwork, query: Set[Variable], evidence: List[(Variable, Double)]) = {
    val combined_probability_table   = bn.factors.reduce(_ * _)
    val conditioned_probability_table = condition(combined_probability_table, evidence)
    val not_in_query                  = conditioned_probability_table.variables.diff(query)
    val factor = not_in_query.foldLeft(conditioned_probability_table) { case (table, variable) =>
      marginalize(table, variable)
    }
    factor.normalize
  }

  def marginalize(f: Factor, variable: Variable): Factor = {
    val ft = f.table.foldLeft(FactorTable()) { case (table, (a, p)) =>
      val filtered_a = a.filter(_._1 != variable)
      val new_p      = table.getOrElse(filtered_a, 0.0) + p
      table + (filtered_a -> new_p)
    }
    Factor(ft)
  }

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
