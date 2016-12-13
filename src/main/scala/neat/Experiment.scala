package neat

trait Experiment {
	def evaluate(individual: Individual): Double
}
