package flappy
import neat.Individual
import neat.Neat

object FlappyNeat extends App {
	implicit val params = neat.Parameters()
	implicit val experiment = FlappyExperiment
	val start = neat.Neat(100).step()
	
	def iter(step: Int, steps: Int, current: Neat): Neat = {
		if(step > steps) current
		else {
			val n = current.step()
			println(s"Step $step:")
			println(s"Best: ${n.best.fitness}, avg fitness: ${n.avgFitness}, avg species age: ${n.avgSpeciesAge}, avg dist: ${n.avgDistance}, n species: ${n.species.length}")
			println(n.population.map(_.fitness).distinct)
			iter(step + 1, steps, n)
		}
	}
	iter(1, 100, start)
}

object FlappyExperiment extends neat.Experiment {
	def evaluate(individual: Individual): Double = {
		val game = new Game()
		game.eval(individual.constructNetwork())
	}
	
	override val inputNodes: Int = 50*50
	override val outputNodes: Int = 1
	override val biasNode: Boolean = true
	override val randNode: Boolean = true
}
