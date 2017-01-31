import neat.{Neat, Parameters}
import util._

object NeatTest {
	implicit val params = Parameters()
	implicit val experiment = TestExperiment()
	val neat = Neat(100).step()
	println(neat.species.flatMap(_.individuals).maxBy(_.fitness).fitness)
	val nneat = iter(50, neat){i => println(i.best.fitness + " - " + i.avgFitness + " - " + i.avgDistance + " - " + i.avgSpeciesAge); i.step()}
	println(nneat.species.flatMap(_.individuals).maxBy(_.fitness).fitness)
}
