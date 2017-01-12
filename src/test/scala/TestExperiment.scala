import neat.{Experiment, Individual}

case class TestExperiment() extends Experiment {
	
	override def evaluate(individual: Individual): Double = individual.conn map (_.weight) sum
	
	override val inputNodes: Int = 2
	override val outputNodes: Int = 1
	override val biasNode: Boolean = true
	override val randNode: Boolean = false
}
