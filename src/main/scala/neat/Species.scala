package neat

import scala.annotation.tailrec
import scala.util.Random

case class Species(repr: Individual, individuals: List[Individual], best: Double = 0, age: Int = 0, dropoffAge: Int = 0)(implicit params: Parameters) {
	lazy val totalFitness: Double = individuals map (_.fitness) sum
	lazy val avgFitness: Double = totalFitness / individuals.length
	lazy val sorted: List[Individual] = individuals.sortBy(_.fitness)
	lazy val maxFitness: Double = individuals.map(_.fitness).max
	
	def dist(o: Individual)(implicit params: Parameters): Double = repr dist o
	
	def accepts(o: Individual)(implicit params: Parameters): Boolean = dist(o) < params.compatibilityThreshold
	
	def add(o: Individual): Species = copy(individuals = o :: individuals)
	lazy val empty: Species = Species(repr, List(), maxFitness.max(best), age + 1, if(maxFitness > best) 0 else age + 1)
	
	@tailrec
	final def breed(offspring: Int, keepBest: Boolean, acc: List[Individual] = List()): List[Individual] = {
		if(offspring <= 0)
			acc
		else if (keepBest) {
			if (individuals.length > 5)
				breed(offspring - 1, false, individuals.maxBy(_.fitness) :: acc)
			else
				breed(offspring, false, acc)
		}
		else {
			val mom = selectRandom(sorted.drop((sorted.length * 0.8).toInt), params.random)
			val dad = selectRandom(sorted.drop((sorted.length * 0.8).toInt), params.random)
			breed(offspring - 1, false, mom.breed(dad) :: acc)
		}
	}
	
	private def selectRandom[E](l: List[E], rand: Random) = l(rand.nextInt(l.length))
}
